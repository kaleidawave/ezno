use source_map::SpanWithSource;

use crate::{
	types::{
		generics::generic_type_arguments::GenericArguments, get_constraint, helpers::into_cases,
		Constant, Constructor, MathematicalOrBitwiseOperation, PartiallyAppliedGenerics, TypeStore,
	},
	TypeId,
};

use super::Type;

pub use crate::utilities::{
	float_range::{FloatRange, InclusiveExclusive},
	modulo_class::ModuloClass,
};

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
pub fn is_tsc_string_intrinsic(id: TypeId) -> bool {
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
	is_tsc_string_intrinsic(id)
		|| is_ezno_number_intrinsic(id)
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
pub fn is_ezno_number_intrinsic(id: TypeId) -> bool {
	matches!(id, TypeId::GREATER_THAN | TypeId::LESS_THAN | TypeId::MULTIPLE_OF)
}

#[must_use]
pub fn get_greater_than(on: TypeId, types: &TypeStore) -> Option<(bool, TypeId)> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: TypeId::GREATER_THAN,
		arguments,
	}) = ty
	{
		let floor = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
		Some((false, floor))
	} else if let Type::And(lhs, rhs) = ty {
		get_greater_than(*lhs, types).or_else(|| get_greater_than(*rhs, types))
	} else if let Type::Or(lhs, rhs) = ty {
		// TODO temp
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
		on: TypeId::LESS_THAN,
		arguments,
	}) = ty
	{
		let floor = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
		Some((false, floor))
	} else if let Type::And(lhs, rhs) = ty {
		get_less_than(*lhs, types).or_else(|| get_less_than(*rhs, types))
	} else if let Type::Or(lhs, rhs) = ty {
		// TODO temp
		get_less_than(*lhs, types).or_else(|| get_less_than(*rhs, types))
	} else if let Type::Constant(crate::Constant::Number(..)) = ty {
		Some((true, on))
	} else {
		None
	}
}

#[must_use]
pub fn range_to_type(range: FloatRange, types: &mut TypeStore) -> TypeId {
	// TODO skip if infinite
	let floor_ty = types.new_constant_type(Constant::Number(range.floor.1));
	let ceiling_ty = types.new_constant_type(Constant::Number(range.ceiling.1));
	let floor = range
		.get_greater_than()
		.map(|_number| new_intrinsic(&Intrinsic::GreaterThan, floor_ty, types));

	let ceiling =
		range.get_less_than().map(|_number| new_intrinsic(&Intrinsic::LessThan, ceiling_ty, types));

	let mut ty = if let (Some(f), Some(c)) = (floor, ceiling) {
		types.new_and_type(f, c)
	} else {
		floor.or(ceiling).unwrap()
	};

	if let InclusiveExclusive::Inclusive = range.floor.0 {
		ty = types.new_or_type(ty, floor_ty);
	}
	if let InclusiveExclusive::Inclusive = range.ceiling.0 {
		ty = types.new_or_type(ty, ceiling_ty);
	}
	ty
}

pub fn modulo_to_type(mod_class: ModuloClass, types: &mut TypeStore) -> TypeId {
	// TODO skip if infinite
	let modulo = types.new_constant_type(Constant::Number(mod_class.modulo));
	let ty = new_intrinsic(&Intrinsic::MultipleOf, modulo, types);
	if mod_class.offset == 0. {
		ty
	} else {
		let offset = types.new_constant_type(Constant::Number(mod_class.offset));
		types.register_type(Type::Constructor(Constructor::BinaryOperator {
			lhs: ty,
			operator: MathematicalOrBitwiseOperation::Add,
			rhs: offset,
			result: TypeId::NUMBER_TYPE,
		}))
	}
}

// TODO offsets as well
#[must_use]
pub fn get_multiple(on: TypeId, types: &TypeStore) -> Option<TypeId> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: TypeId::MULTIPLE_OF,
		arguments,
	}) = ty
	{
		arguments.get_structure_restriction(TypeId::NUMBER_GENERIC)
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
			on: TypeId::MULTIPLE_OF | TypeId::GREATER_THAN | TypeId::LESS_THAN,
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

pub fn new_intrinsic(intrinsic: &Intrinsic, argument: TypeId, types: &mut TypeStore) -> TypeId {
	use source_map::Nullable;
	let (on, to_pair) =
		match intrinsic {
			Intrinsic::Uppercase => (TypeId::STRING_UPPERCASE, TypeId::STRING_GENERIC),
			Intrinsic::Lowercase => (TypeId::STRING_LOWERCASE, TypeId::STRING_GENERIC),
			Intrinsic::Capitalize => (TypeId::STRING_CAPITALIZE, TypeId::STRING_GENERIC),
			Intrinsic::Uncapitalize => (TypeId::STRING_UNCAPITALIZE, TypeId::STRING_GENERIC),
			Intrinsic::NoInfer => (TypeId::NO_INFER, TypeId::T_TYPE),
			Intrinsic::Literal => (TypeId::LITERAL_RESTRICTION, TypeId::T_TYPE),
			Intrinsic::LessThan => {
				let arguments = GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
					TypeId::NUMBER_GENERIC,
					(argument, SpanWithSource::NULL),
				)]));

				return types.register_type(Type::PartiallyAppliedGenerics(
					PartiallyAppliedGenerics { on: TypeId::LESS_THAN, arguments },
				));
			}
			Intrinsic::GreaterThan => {
				let arguments = GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
					TypeId::NUMBER_GENERIC,
					(argument, SpanWithSource::NULL),
				)]));

				return types.register_type(Type::PartiallyAppliedGenerics(
					PartiallyAppliedGenerics { on: TypeId::GREATER_THAN, arguments },
				));
			}
			Intrinsic::MultipleOf => (TypeId::MULTIPLE_OF, TypeId::NUMBER_GENERIC),
			Intrinsic::Exclusive => (TypeId::EXCLUSIVE_RESTRICTION, TypeId::T_TYPE),
			Intrinsic::Not => {
				// Double negation
				if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: TypeId::NOT_RESTRICTION,
					arguments: GenericArguments::ExplicitRestrictions(args),
				}) = types.get_type_by_id(argument)
				{
					return args.get(&TypeId::T_TYPE).unwrap().0;
				}

				(TypeId::NOT_RESTRICTION, TypeId::T_TYPE)
			}
			Intrinsic::CaseInsensitive => (TypeId::CASE_INSENSITIVE, TypeId::STRING_GENERIC),
		};

	let arguments = GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
		to_pair,
		(argument, SpanWithSource::NULL),
	)]));

	types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }))
}

#[must_use]
pub fn get_range_and_mod_class(
	ty: TypeId,
	types: &TypeStore,
) -> (Option<FloatRange>, Option<ModuloClass>) {
	let cases = into_cases(ty, types);
	let interesting = cases.iter().enumerate().find_map(|(idx, case)| {
		case.0
			.iter()
			.copied()
			.try_fold(Vec::new(), |mut acc, ty| {
				if let Ok(item) = PureNumberIntrinsic::try_from_type(ty.0, types) {
					acc.push(item);
					Some(acc)
				} else {
					None
				}
			})
			.map(|value| (idx, value))
	});

	if let Some((idx, interesting)) = interesting {
		let mut range: Option<FloatRange> = None;
		let mut modulo_class: Option<ModuloClass> = None;
		for number_condition in interesting {
			match number_condition {
				PureNumberIntrinsic::GreaterThan(greater_than) => {
					range = Some(match range {
						None => FloatRange::new_greater_than(greater_than),
						Some(mut range) => {
							range.floor.1 = range.floor.1.max(greater_than);
							range
						}
					});
				}
				PureNumberIntrinsic::LessThan(less_than) => {
					range = Some(match range {
						None => FloatRange::new_less_than(less_than),
						Some(mut range) => {
							range.ceiling.1 = range.ceiling.1.min(less_than);
							range
						}
					});
				}
				PureNumberIntrinsic::Modulo { modulo, offset } => {
					modulo_class = Some(if modulo_class.is_none() {
						ModuloClass::new(modulo, offset)
					} else {
						crate::utilities::notify!("TODO intersection");
						return (None, None);
					});
				}
			}
		}

		for (other_idx, case) in cases.into_iter().enumerate() {
			// Skip the interesting case
			if idx == other_idx {
				continue;
			}
			// Test whether all cases fit inside this new condition. Also modify ranges for inclusion if not
			if let [value] = &case.0[..] {
				if let Type::Constant(Constant::Number(num)) = types.get_type_by_id(value.0) {
					let num = *num;
					if let Some(ref modulo_class) = modulo_class {
						if !modulo_class.contains(num) {
							return (None, None);
						}
					}
					if let Some(ref mut range) = range {
						if range.floor.1 == num {
							range.floor.0 = InclusiveExclusive::Inclusive;
						} else if range.ceiling.1 == num {
							range.ceiling.0 = InclusiveExclusive::Inclusive;
						} else if !range.contains(num) {
							crate::utilities::notify!(
								"Here, number not contained in current range"
							);
							return (None, None);
						}
					}
				}
			} else {
				return (None, None);
			}
		}
		(range, modulo_class)
	} else if let Type::Constant(Constant::Number(num)) = types.get_type_by_id(ty) {
		(Some(FloatRange::new_single(*num)), None)
	} else {
		// crate::utilities::notify!("Not interesting or constant {:?}", ty);
		(None, None)
	}
}

/// Unit. No combinations at this point
#[derive(Debug)]
pub enum PureNumberIntrinsic {
	GreaterThan(f64),
	LessThan(f64),
	Modulo { modulo: f64, offset: f64 },
}

impl PureNumberIntrinsic {
	pub fn try_from_type(ty: TypeId, types: &TypeStore) -> Result<PureNumberIntrinsic, ()> {
		let ty = types.get_type_by_id(ty);
		if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: on @ (TypeId::GREATER_THAN | TypeId::LESS_THAN | TypeId::MULTIPLE_OF),
			arguments,
		}) = ty
		{
			let arg = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
			if let Type::Constant(Constant::Number(number)) = types.get_type_by_id(arg) {
				match *on {
					TypeId::GREATER_THAN => Ok(PureNumberIntrinsic::GreaterThan(*number)),
					TypeId::LESS_THAN => Ok(PureNumberIntrinsic::LessThan(*number)),
					TypeId::MULTIPLE_OF => Ok(PureNumberIntrinsic::Modulo {
						modulo: *number,
						offset: 0f64.try_into().unwrap(),
					}),
					_ => todo!(),
				}
			} else {
				Err(())
			}
		} else if let Type::Constructor(Constructor::BinaryOperator {
			lhs,
			operator: MathematicalOrBitwiseOperation::Add,
			rhs,
			..
		}) = ty
		{
			let lhs_ty = types.get_type_by_id(super::get_constraint(*lhs, types).unwrap_or(*lhs));
			crate::utilities::notify!("{:?}", lhs_ty);
			let lhs_modulo = if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::MULTIPLE_OF,
				arguments,
			}) = lhs_ty
			{
				let arg = arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
				if let Type::Constant(Constant::Number(number)) = types.get_type_by_id(arg) {
					Some(number)
				} else {
					None
				}
			} else {
				None
			};

			if let (Some(modulo), Type::Constant(Constant::Number(offset))) =
				(lhs_modulo, types.get_type_by_id(*rhs))
			{
				Ok(PureNumberIntrinsic::Modulo { modulo: *modulo, offset: *offset })
			} else {
				Err(())
			}
		} else {
			// crate::utilities::notify!("err here {:?}", ty);
			Err(())
		}
	}
}

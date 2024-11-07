use super::{Constant, PartiallyAppliedGenerics, Type, TypeId, TypeStore};
use crate::context::InformationChain;

/// For equality + [`crate::intrinsics::Intrinsics::Not`]
///
/// TODO slices
/// TODO properties
/// TODO <https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.disjoint_compl_left_iff_subset> (which references subtyping) and <https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.disjoint_compl_right_iff_subset> (via commutativity of this operation)
///
/// Could shrink some logic here but is more readable verbose
pub fn types_are_disjoint(
	lhs: TypeId,
	rhs: TypeId,
	already_checked: &mut Vec<(TypeId, TypeId)>,
	information: &impl InformationChain,
	types: &TypeStore,
) -> bool {
	// crate::utilities::notify!("are disjoint? {:?}", (lhs, rhs));

	if lhs == rhs || lhs == TypeId::ANY_TYPE || rhs == TypeId::ANY_TYPE {
		false
	} else if already_checked.iter().any(|pair| *pair == (lhs, rhs)) {
		// TODO explain why `true`
		true
	} else {
		let lhs_ty = types.get_type_by_id(lhs);
		let rhs_ty = types.get_type_by_id(rhs);

		// Order of these branches matter
		if let Type::Or(lhs_lhs, lhs_rhs) = lhs_ty {
			types_are_disjoint(*lhs_lhs, rhs, already_checked, information, types)
				&& types_are_disjoint(*lhs_rhs, rhs, already_checked, information, types)
		} else if let Type::Or(rhs_lhs, rhs_rhs) = rhs_ty {
			types_are_disjoint(lhs, *rhs_lhs, already_checked, information, types)
				&& types_are_disjoint(lhs, *rhs_rhs, already_checked, information, types)
		} else if let Type::AliasTo { to, parameters: None, name: _ } = lhs_ty {
			// TODO temp fix, need infer ANY
			if matches!(*to, TypeId::ANY_TYPE) {
				true
			} else {
				types_are_disjoint(*to, rhs, already_checked, information, types)
			}
		} else if let Type::AliasTo { to, parameters: None, name: _ } = rhs_ty {
			if matches!(*to, TypeId::ANY_TYPE) {
				true
			} else {
				types_are_disjoint(lhs, *to, already_checked, information, types)
			}
		} else if let (
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::ARRAY_TYPE,
				arguments: _arguments,
			}),
			Type::Object(super::ObjectNature::RealDeal),
		) = (lhs_ty, rhs_ty)
		{
			let rhs_prototype =
				information.get_chain_of_info().find_map(|info| info.prototypes.get(&rhs).copied());
			// {
			// 		if let Some(lhs_prototype) = info.prototypes.get(&lhs).copied() {
			// 	let rhs_prototype = information.get_prototype_of(rhs);

			// TODO leaving arguments out of picture for now
			rhs_prototype != Some(TypeId::ARRAY_TYPE)
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = lhs_ty
		{
			use super::subtyping;
			let lhs_inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			let mut state = subtyping::State {
				// TODO
				already_checked: already_checked.clone(),
				mode: Default::default(),
				contributions: None,
				others: subtyping::SubTypingOptions { allow_errors: true },
				object_constraints: None,
			};

			// crate::utilities::notify!("{:?}", (lhs, lhs_inner));

			subtyping::type_is_subtype(lhs_inner, rhs, &mut state, information, types).is_subtype()
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = rhs_ty
		{
			use super::subtyping;
			let rhs_inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			let mut state = subtyping::State {
				// TODO
				already_checked: already_checked.clone(),
				mode: Default::default(),
				contributions: None,
				others: subtyping::SubTypingOptions { allow_errors: true },
				object_constraints: None,
			};

			crate::utilities::notify!("{:?}", (rhs, rhs_inner));

			subtyping::type_is_subtype(rhs_inner, lhs, &mut state, information, types).is_subtype()
		} else if let Type::And(rhs_lhs, rhs_rhs) = rhs_ty {
			types_are_disjoint(lhs, *rhs_lhs, already_checked, information, types)
				|| types_are_disjoint(lhs, *rhs_rhs, already_checked, information, types)
		} else if let Type::And(lhs_lhs, lhs_rhs) = lhs_ty {
			types_are_disjoint(*lhs_lhs, rhs, already_checked, information, types)
				|| types_are_disjoint(*lhs_rhs, rhs, already_checked, information, types)
		} else if let Some(lhs) = super::get_constraint(lhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Some(rhs) = super::get_constraint(rhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics { on: TypeId::MULTIPLE_OF, arguments: _ },
		) = lhs_ty
		{
			number_modulo_disjoint(args, rhs, types)
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics { on: TypeId::MULTIPLE_OF, arguments: _ },
		) = rhs_ty
		{
			number_modulo_disjoint(args, lhs, types)
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics {
				on: TypeId::GREATER_THAN | TypeId::LESS_THAN,
				arguments: _,
			},
		) = rhs_ty
		{
			number_range_disjoint(args, lhs, types)
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics {
				on: TypeId::GREATER_THAN | TypeId::LESS_THAN,
				arguments: _,
			},
		) = lhs_ty
		{
			number_range_disjoint(args, rhs, types)
		} else if let Type::Constant(lhs_cst) = lhs_ty {
			if let Type::Constant(rhs_cst) = rhs_ty {
				lhs_cst != rhs_cst
			} else {
				types_are_disjoint(
					lhs_cst.get_backing_type(),
					rhs,
					already_checked,
					information,
					types,
				)
			}
		} else if let Type::Constant(rhs_cst) = rhs_ty {
			types_are_disjoint(rhs_cst.get_backing_type(), lhs, already_checked, information, types)
		} else if let Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation(
			_properties,
		)) = lhs_ty
		{
			// TODO check properties
			false
		} else if let Type::Object(crate::types::ObjectNature::AnonymousTypeAnnotation(
			_properties,
		)) = rhs_ty
		{
			// TODO check properties
			false
		} else {
			crate::utilities::notify!(
				"{:?} cap {:?} == empty ? cases. Might be missing, calling disjoint",
				lhs_ty,
				rhs_ty
			);
			true
		}
	}
}

fn number_modulo_disjoint(
	this: &PartiallyAppliedGenerics,
	other: TypeId,
	types: &TypeStore,
) -> bool {
	let PartiallyAppliedGenerics { arguments, .. } = this;
	let other = types.get_type_by_id(other);
	// Little bit complex here because dealing with decimal types, not integers
	if let (Type::Constant(Constant::Number(other)), Type::Constant(Constant::Number(this))) = (
		other,
		types.get_type_by_id(arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap()),
	) {
		let result = other % this != 0.;
		// crate::utilities::notify!("{:?} {:?}", lhs, rhs);
		result
	} else {
		// crate::utilities::notify!("Here {:?}", lhs);
		true
	}
}

fn number_range_disjoint(
	this: &PartiallyAppliedGenerics,
	other: TypeId,
	types: &TypeStore,
) -> bool {
	let PartiallyAppliedGenerics { on, arguments, .. } = this;
	let greater_than = *on == TypeId::GREATER_THAN;
	let this_ty =
		types.get_type_by_id(arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap());
	if let Type::Constant(Constant::Number(this)) = this_ty {
		let other_ty = types.get_type_by_id(other);
		if let Type::Constant(Constant::Number(other)) = other_ty {
			crate::utilities::notify!("{:?} {} {}", on, other, this);
			if greater_than {
				other < this
			} else {
				other > this
			}
		} else {
			crate::utilities::notify!("Unsure here {:?}", (other_ty, this_ty));
			false
		}
	} else {
		crate::utilities::notify!("Unsure here");
		false
	}
}

// fn todo() {
// 	// else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
// 	// 	on: TypeId::GREATER_THAN
// 	// 	arguments: _,
// 	// }) = lhs_ty
// 	// {
// 	// 	let range = super::intrinsics::get_range(lhs, types).unwrap();
// 	// 	if let Some(rhs_range) = super::intrinsics::get_range(rhs, types) {
// 	// 		let overlap = range.overlaps(rhs_range);
// 	// 		crate::utilities::notify!("{:?}", overlap);
// 	// 		!overlap
// 	// 	} else {
// 	// 		crate::utilities::notify!("Here");
// 	// 		true
// 	// 	}
// 	// } else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
// 	// 	on: TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
// 	// 	arguments: _,
// 	// }) = rhs_ty
// 	// {
// 	// 	let range = super::intrinsics::get_range(rhs, types).unwrap();
// 	// 	if let Some(lhs_range) = super::intrinsics::get_range(lhs, types) {
// 	// 		let overlap = range.overlaps(lhs_range);
// 	// 		crate::utilities::notify!("{:?}", overlap);
// 	// 		!overlap
// 	// 	} else {
// 	// 		crate::utilities::notify!("Here");
// 	// 		true
// 	// 	}
// 	// }
// }

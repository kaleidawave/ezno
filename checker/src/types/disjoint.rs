use super::PartiallyAppliedGenerics;
use crate::{context::InformationChain, types::TypeStore, Type, TypeId};

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
	crate::utilities::notify!("are disjoint? {:?}", (lhs, rhs));

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
		} else if let Type::And(lhs_lhs, lhs_rhs) = lhs_ty {
			types_are_disjoint(*lhs_lhs, rhs, already_checked, information, types)
				|| types_are_disjoint(*lhs_rhs, rhs, already_checked, information, types)
		} else if let Type::Or(rhs_lhs, rhs_rhs) = rhs_ty {
			types_are_disjoint(lhs, *rhs_lhs, already_checked, information, types)
				&& types_are_disjoint(lhs, *rhs_rhs, already_checked, information, types)
		} else if let Type::And(rhs_lhs, rhs_rhs) = rhs_ty {
			types_are_disjoint(lhs, *rhs_lhs, already_checked, information, types)
				|| types_are_disjoint(lhs, *rhs_rhs, already_checked, information, types)
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
			let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			let mut state = subtyping::State {
				// TODO
				already_checked: already_checked.clone(),
				mode: Default::default(),
				contributions: None,
				others: subtyping::SubTypingOptions { allow_errors: false },
				object_constraints: None,
			};

			crate::utilities::notify!("{:?}", (lhs, inner));

			subtyping::type_is_subtype(rhs, inner, &mut state, information, types).is_subtype()
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = rhs_ty
		{
			use super::subtyping;
			let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
			let mut state = subtyping::State {
				// TODO
				already_checked: already_checked.clone(),
				mode: Default::default(),
				contributions: None,
				others: subtyping::SubTypingOptions { allow_errors: false },
				object_constraints: None,
			};

			crate::utilities::notify!("{:?}", (lhs, inner));

			subtyping::type_is_subtype(lhs, inner, &mut state, information, types).is_subtype()
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
			arguments: _,
		}) = lhs_ty
		{
			let range = super::intrinsics::get_range(lhs, types).unwrap();
			if let Some(rhs_range) = super::intrinsics::get_range(rhs, types) {
				!range.overlaps(rhs_range)
			} else {
				true
			}
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
			arguments: _,
		}) = rhs_ty
		{
			let range = super::intrinsics::get_range(rhs, types).unwrap();
			if let Some(lhs_range) = super::intrinsics::get_range(lhs, types) {
				!range.overlaps(lhs_range)
			} else {
				true
			}
		} else if let Some(lhs) = super::get_constraint(lhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Some(rhs) = super::get_constraint(rhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Type::Constant(lhs_cst) = lhs_ty {
			if let Type::Constant(rhs_cst) = rhs_ty {
				lhs_cst != rhs_cst
			} else {
				types_are_disjoint(
					lhs_cst.get_backing_type_id(),
					rhs,
					already_checked,
					information,
					types,
				)
			}
		} else if let Type::Constant(rhs_cst) = rhs_ty {
			types_are_disjoint(
				rhs_cst.get_backing_type_id(),
				lhs,
				already_checked,
				information,
				types,
			)
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

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
	left: TypeId,
	right: TypeId,
	already_checked: &mut Vec<(TypeId, TypeId)>,
	information: &impl InformationChain,
	types: &TypeStore,
) -> bool {
	if left == right || left == TypeId::ANY_TYPE || right == TypeId::ANY_TYPE {
		false
	} else if already_checked.iter().any(|pair| *pair == (left, right)) {
		// TODO explain why `true`
		true
	} else {
		let left_ty = types.get_type_by_id(left);
		let right_ty = types.get_type_by_id(right);

		// Order of these branches matter
		if let Type::Or(left_left, left_right) = left_ty {
			types_are_disjoint(*left_left, right, already_checked, information, types)
				&& types_are_disjoint(*left_right, right, already_checked, information, types)
		} else if let Type::And(left_left, left_right) = left_ty {
			types_are_disjoint(*left_left, right, already_checked, information, types)
				|| types_are_disjoint(*left_right, right, already_checked, information, types)
		} else if let Type::Or(right_left, right_right) = right_ty {
			types_are_disjoint(left, *right_left, already_checked, information, types)
				&& types_are_disjoint(left, *right_right, already_checked, information, types)
		} else if let Type::And(right_left, right_right) = right_ty {
			types_are_disjoint(left, *right_left, already_checked, information, types)
				|| types_are_disjoint(left, *right_right, already_checked, information, types)
		} else if let Type::AliasTo { to, parameters: None, name: _ } = left_ty {
			// TODO temp fix, need infer ANY
			if matches!(*to, TypeId::ANY_TYPE) {
				true
			} else {
				types_are_disjoint(*to, right, already_checked, information, types)
			}
		} else if let Type::AliasTo { to, parameters: None, name: _ } = right_ty {
			if matches!(*to, TypeId::ANY_TYPE) {
				true
			} else {
				types_are_disjoint(left, *to, already_checked, information, types)
			}
		} else if let (
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::ARRAY_TYPE,
				arguments: _arguments,
			}),
			Type::Object(super::ObjectNature::RealDeal),
		) = (left_ty, right_ty)
		{
			let rhs_prototype = information
				.get_chain_of_info()
				.find_map(|info| info.prototypes.get(&right).copied());
			// {
			// 		if let Some(lhs_prototype) = info.prototypes.get(&lhs).copied() {
			// 	let rhs_prototype = information.get_prototype_of(right);

			// TODO leaving arguments out of picture for now
			rhs_prototype != Some(TypeId::ARRAY_TYPE)
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = left_ty
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

			subtyping::type_is_subtype(right, inner, &mut state, information, types).is_subtype()
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments,
		}) = right_ty
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

			subtyping::type_is_subtype(left, inner, &mut state, information, types).is_subtype()
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
			arguments: _,
		}) = left_ty
		{
			let range = super::intrinsics::get_range(left, types).unwrap();
			if let Some(right_range) = super::intrinsics::get_range(right, types) {
				!range.overlaps(right_range)
			} else {
				true
			}
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
			arguments: _,
		}) = right_ty
		{
			let range = super::intrinsics::get_range(right, types).unwrap();
			if let Some(left_range) = super::intrinsics::get_range(left, types) {
				!range.overlaps(left_range)
			} else {
				true
			}
		} else if let Type::Constant(left_cst) = left_ty {
			if let Type::Constant(right_cst) = right_ty {
				left_cst != right_cst
			} else {
				types_are_disjoint(
					left_cst.get_backing_type_id(),
					right,
					already_checked,
					information,
					types,
				)
			}
		} else if let Type::Constant(right_cst) = right_ty {
			types_are_disjoint(
				right_cst.get_backing_type_id(),
				left,
				already_checked,
				information,
				types,
			)
		} else if let Some(left) = super::get_constraint(left, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(left, right, already_checked, information, types)
		} else if let Some(right) = super::get_constraint(right, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(left, right, already_checked, information, types)
		} else {
			crate::utilities::notify!(
				"{:?} cap {:?} == empty ? cases. Might be missing, calling disjoint",
				left_ty,
				right_ty
			);
			true
		}
	}
}

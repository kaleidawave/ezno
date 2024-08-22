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

		// if let Type::Constructor(Constructor::KeyOf(_)) = left_ty {
		// 	todo!("get property != ")
		// } else
		if let Type::Constant(left_cst) = left_ty {
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
		} else if let Type::Or(left_left, left_right) = left_ty {
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
			arguments: _arguments,
		}) = left_ty
		{
			crate::utilities::notify!("TODO not restriction requires subtyping, skipping for now");
			false
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::NOT_RESTRICTION,
			arguments: _arguments,
		}) = right_ty
		{
			crate::utilities::notify!("TODO not restriction requires subtyping, skipping for now");
			false
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

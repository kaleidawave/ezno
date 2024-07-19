use crate::{context::information::InformationChain, types::TypeStore, Type, TypeId};

/// For equality + [`crate::intrinsics::Intrinsics::Not`]
///
/// TODO slices
/// TODO properties
/// TODO <https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.disjoint_compl_left_iff_subset> (which references subtyping) and <https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Set/Basic.html#Set.disjoint_compl_right_iff_subset> (via commutativity of this operation)
pub fn types_are_disjoint(
	left: TypeId,
	right: TypeId,
	already_checked: &mut Vec<(TypeId, TypeId)>,
	information: &impl InformationChain,
	types: &TypeStore,
) -> bool {
	if left == right {
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
				left_cst.get_backing_type_id() != right
			}
		} else if let Type::Constant(right_cst) = right_ty {
			right_cst.get_backing_type_id() != left
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
		} else if let (Type::Object(super::ObjectNature::RealDeal), _)
		| (_, Type::Object(super::ObjectNature::RealDeal)) = (left_ty, right_ty)
		{
			true
		} else {
			crate::utilities::notify!(
				"{:?} cap {:?} == empty ? cases. Might be missing, calling disjoint",
				left_ty,
				right_ty
			);
			false
		}
	}
}

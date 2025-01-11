use super::{
	helpers, Constant, Constructor, MathematicalOrBitwiseOperation, PartiallyAppliedGenerics, Type,
	TypeId, TypeStore,
};
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
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics { on: TypeId::MULTIPLE_OF, arguments: _ },
		) = lhs_ty
		{
			// TODO also offset
			number_modulo_disjoint(args, rhs, types)
		} else if let Type::PartiallyAppliedGenerics(
			args @ PartiallyAppliedGenerics { on: TypeId::MULTIPLE_OF, arguments: _ },
		) = rhs_ty
		{
			// TODO also offset
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
		} else if let Type::Constructor(Constructor::BinaryOperator {
			lhs: _lhs,
			operator: MathematicalOrBitwiseOperation::Modulo,
			rhs,
			result: _,
		}) = lhs_ty
		{
			if let (
				Type::Constant(Constant::Number(lhs_mod)),
				Type::Constant(Constant::Number(num)),
			) = (types.get_type_by_id(*rhs), rhs_ty)
			{
				crate::utilities::notify!("{:?}", (num, lhs_mod));
				// Modulos return negative for negative number :(
				// Checking whether out of range here
				num.abs() > *lhs_mod
			} else {
				false
			}
		} else if let Type::Constructor(Constructor::BinaryOperator {
			lhs: _lhs,
			operator: MathematicalOrBitwiseOperation::Modulo,
			rhs,
			result: _,
		}) = rhs_ty
		{
			if let (
				Type::Constant(Constant::Number(num)),
				Type::Constant(Constant::Number(rhs_mod)),
			) = (types.get_type_by_id(*rhs), lhs_ty)
			{
				crate::utilities::notify!("{:?}", (num, rhs_mod));
				// Modulos return negative for negative number :(
				// Checking whether out of range here
				num.abs() > *rhs_mod
			} else {
				false
			}
		} else if let Type::Constructor(Constructor::BinaryOperator {
			operator: MathematicalOrBitwiseOperation::Add,
			result: TypeId::STRING_TYPE,
			..
		}) = rhs_ty
		{
			let lhs = helpers::TemplatelLiteralExpansion::from_type(lhs, types);
			let rhs = helpers::TemplatelLiteralExpansion::from_type(rhs, types);
			lhs.is_disjoint(&rhs)
		} else if let Type::Constructor(Constructor::BinaryOperator {
			operator: MathematicalOrBitwiseOperation::Add,
			result: TypeId::STRING_TYPE,
			..
		}) = lhs_ty
		{
			let lhs = helpers::TemplatelLiteralExpansion::from_type(lhs, types);
			let rhs = helpers::TemplatelLiteralExpansion::from_type(rhs, types);
			lhs.is_disjoint(&rhs)
		} else if let Some(lhs) = super::get_constraint(lhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Some(rhs) = super::get_constraint(rhs, types) {
			// TODO not sure whether these should be here?
			types_are_disjoint(lhs, rhs, already_checked, information, types)
		} else if let Type::Constant(lhs_cst) = lhs_ty {
			if let Type::Constant(rhs_cst) = rhs_ty {
				!lhs_cst.equals(rhs_cst)
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
	crate::utilities::notify!("Here number_modulo_disjoint");
	let PartiallyAppliedGenerics { arguments, .. } = this;
	let other_ty = types.get_type_by_id(other);
	let argument =
		types.get_type_by_id(arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap());

	let Type::Constant(Constant::Number(argument)) = argument else {
		crate::utilities::notify!("Gets complex here");
		return false;
	};

	let offset = 0f64.try_into().unwrap();
	let this = crate::utilities::modulo_class::ModuloClass::new(*argument, offset);

	// Little bit complex here because dealing with decimal types, not integers
	if let Type::Constant(Constant::Number(other)) = other_ty {
		!this.contains(*other)
	} else {
		let (range, modulo_class) = super::intrinsics::get_range_and_mod_class(other, types);
		crate::utilities::notify!("Disjoint with modulo, {:?}, {:?}", range, modulo_class);
		if let Some(range) = range {
			return !range.contains_multiple_of(*argument);
		}
		if let Some(modulo_class) = modulo_class {
			return this.disjoint(modulo_class);
		}
		crate::utilities::notify!("Here {:?}", other_ty);
		false
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

use source_map::SpanWithSource;

use crate::{
	types::{get_constraint, StructureGenerics, TypeStore},
	CheckingData, Environment, Type, TypeId,
};

use self::objects::SpecialObjects;

/// Contains implementations of specific JavaScript items and how Ezno handles them.
/// Contains
/// - Helper / abstracting functions for synthesising
/// Does not contain
/// - Logic stuff
/// - Context
/// - Internal structures
pub mod assignments;
pub mod constant_functions;
pub mod functions;
pub mod iteration;
pub mod modules;
pub mod objects;
pub mod operations;
pub mod template_literal;
pub mod variables;

pub fn type_of_operator(on: TypeId, types: &mut TypeStore) -> TypeId {
	if let Some(constraint) = get_constraint(on, types) {
		let name = match constraint {
			TypeId::NUMBER_TYPE => "number",
			TypeId::STRING_TYPE => "string",
			TypeId::BOOLEAN_TYPE => "boolean",
			TypeId::SYMBOL_TYPE => "symbol",
			_constraint => {
				return types.register_type(crate::Type::Constructor(
					crate::types::Constructor::TypeOperator(crate::types::TypeOperator::TypeOf(on)),
				))
			}
		};
		// TODO could Cow or something to not allocate?
		types.new_constant_type(crate::Constant::String(name.to_owned()))
	} else {
		let ty = types.get_type_by_id(on);
		if let crate::Type::Constant(cst) = ty {
			let name = match cst {
				crate::Constant::NaN | crate::Constant::Number(_) => "number",
				crate::Constant::String(_) => "string",
				crate::Constant::Boolean(_) => "boolean",
				crate::Constant::Symbol { key: _ } => "symbol",
				crate::Constant::Undefined => "undefined",
				crate::Constant::Null => "object",
			};
			// TODO could Cow or something to not allocate?
			types.new_constant_type(crate::Constant::String(name.to_owned()))
		} else if let crate::Type::SpecialObject(SpecialObjects::Function(..)) = ty {
			types.new_constant_type(crate::Constant::String("function".to_owned()))
		} else if let crate::Type::Object(..) | crate::Type::SpecialObject(..) = ty {
			types.new_constant_type(crate::Constant::String("object".to_owned()))
		} else {
			crate::utils::notify!("Cannot `typeof {:?}`", on);
			TypeId::ERROR_TYPE
		}
	}
}

pub fn as_cast(on: TypeId, cast_to: TypeId, types: &mut TypeStore) -> Result<TypeId, ()> {
	use crate::types::{Constructor, PolyNature};

	fn can_cast_type(ty: &Type) -> bool {
		match ty {
			// TODO some of these are more correct than the others
			crate::Type::RootPolyType(_rpt) => true,
			crate::Type::Constructor(constr) => match constr {
				Constructor::CanonicalRelationOperator { .. }
				| Constructor::UnaryOperator { .. }
				| Constructor::StructureGenerics(_)
				| Constructor::BinaryOperator { .. } => false,
				Constructor::TypeOperator(_) => todo!(),
				Constructor::TypeRelationOperator(_) => todo!(),
				Constructor::Awaited { .. }
				| Constructor::ConditionalResult { .. }
				| Constructor::Image { .. }
				| Constructor::Property { .. } => true,
			},
			_ => false,
		}
	}

	let can_cast = on == TypeId::ERROR_TYPE || can_cast_type(types.get_type_by_id(on));

	if can_cast {
		// TSC compat around `any`
		let cast_to = if cast_to == TypeId::ANY_TYPE { TypeId::ERROR_TYPE } else { cast_to };

		Ok(types.register_type(Type::RootPolyType(PolyNature::Open(cast_to))))
	} else {
		Err(())
	}
}

/// TODO await ors etc
pub fn await_expression<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	on: TypeId,
	_environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	position: SpanWithSource,
) -> TypeId {
	if let Some(constraint) = get_constraint(on, &checking_data.types) {
		// TODO mark type as awaited
		let inner_type = get_promise_value(constraint, &checking_data.types);
		match inner_type {
			Some(result) => {
				crate::utils::notify!("Queue await effect");
				checking_data.types.register_type(Type::Constructor(
					crate::types::Constructor::Awaited { on, result },
				))
			}
			None => {
				checking_data.raise_unimplemented_error(
					"await has no effect (or awaited expression is more complex)",
					position,
				);
				on
			}
		}
	} else {
		checking_data.raise_unimplemented_error("await on object", position);
		TypeId::ERROR_TYPE
	}
}

fn get_promise_value(constraint: TypeId, types: &TypeStore) -> Option<TypeId> {
	if let Type::Constructor(crate::types::Constructor::StructureGenerics(StructureGenerics {
		on: TypeId::PROMISE_TYPE,
		arguments,
	})) = types.get_type_by_id(constraint)
	{
		let result = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
		Some(result)
	} else {
		None
	}
}

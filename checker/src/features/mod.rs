use crate::{
	types::{get_constraint, TypeStore},
	TypeId,
};

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
		} else if let crate::Type::Function(..) = ty {
			types.new_constant_type(crate::Constant::String("function".to_owned()))
		} else if let crate::Type::Object(..) | crate::Type::SpecialObject(..) = ty {
			types.new_constant_type(crate::Constant::String("object".to_owned()))
		} else {
			crate::utils::notify!("Cannot `typeof {:?}`", on);
			TypeId::ERROR_TYPE
		}
	}
}

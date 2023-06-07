use parser::{
	expressions::object_literal::{ObjectLiteral, ObjectLiteralMember},
	ASTNode, Chain,
};
use temporary_annex::Annex;

use crate::{
	errors::TypeCheckError,
	structures::{objects::ObjectBuilder, variables::VariableWithValue},
	synthesis::property_key_as_type,
	types::Constant,
	CheckingData, Environment, TypeId,
};

use super::{synthesize_expression, synthesize_function};

pub(crate) fn synthesize_object_literal<T: crate::FSResolver>(
	ObjectLiteral { members, .. }: &mut ObjectLiteral,
	checking_data: &mut CheckingData<T>,
	environment: &mut Environment,
	chain: &mut Annex<Chain>,
) -> TypeId {
	let mut object_builder = ObjectBuilder::new(None, &mut checking_data.types, environment);

	for member in members.iter_mut() {
		match member {
			ObjectLiteralMember::SpreadExpression(spread, _) => {
				checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
					thing: "spread in object literal",
					at: spread.get_position().into_owned(),
				});
			}
			ObjectLiteralMember::Shorthand(name, position, _, _) => {
				let key = checking_data.types.new_constant_type(Constant::String(name.clone()));
				let get_variable =
					environment.get_variable_or_alternatives(name, &mut checking_data.types);
				let value = match get_variable {
					Ok(VariableWithValue(_variable, value)) => value,
					Err(err) => {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::CouldNotFindVariable {
								variable: err.name,
								possibles: err.possibles,
								position: position.clone(),
							},
						);
						TypeId::ERROR_TYPE
					}
				};

				object_builder.append(environment, key, value);
			}
			ObjectLiteralMember::Property(key, expression, _) => {
				let key =
					property_key_as_type(key.get_ast(), environment, &mut checking_data.types);
				let value = synthesize_expression(expression, environment, checking_data, chain);

				object_builder.append(environment, key, value);

				// let property_name: PropertyName<'static> = property_key.into();

				// TODO a little temp
				// checking_data
				//     .type_mappings
				//     .properties_to_types
				//     .insert(property_key.get_ast().get_property_id(), value.clone());

				// (
				//     property_name,
				//     Property {
				//         ty: value.into(),
				//         writeable: true,
				//         configurable: true,
				//         enumerable: true,
				//     },
				// )
			}
			// ObjectLiteralMember::EmptyDataMember { data } => {
			// 	if let Some(FunctionHoistedHere(function_id)) = data.downcast_ref() {
			// 		let (func, _) = checking_data
			// 			.hoisted_functions
			// 			.uncalled_function_pool
			// 			.get(function_id)
			// 			.unwrap();

			// 		if let HoistedFunction::HoistedObjectLiteralMethod(object) = func {
			// 			let key = property_key_as_type(object.key.get_ast(), environment);
			// 			let function_type = environment.new_function_type(
			// 				function_id.clone().into(),
			// 				object.get_set_generator_or_none,
			// 			);

			// 			object_builder.append(environment, key, function_type);
			// 		} else {
			// 			unreachable!()
			// 		}
			// 	} else {
			// 		unreachable!("Unknown data member in interface")
			// 	}
			// }
			ObjectLiteralMember::Method(method) => {
				let func = synthesize_function(method, environment, checking_data);

				let key = property_key_as_type(
					method.name.get_ast(),
					environment,
					&mut checking_data.types,
				);

				match method.header {
					parser::GetSetGeneratorOrNone::Get(_) => todo!(),
					parser::GetSetGeneratorOrNone::Set(_) => todo!(),
					parser::GetSetGeneratorOrNone::Generator(_) => todo!(),
					parser::GetSetGeneratorOrNone::GeneratorStar(_) => todo!(),
					parser::GetSetGeneratorOrNone::None => {
						let value = checking_data.types.new_type(crate::types::Type::Function(
							func,
							crate::types::FunctionNature::Source(method.function_id, None, None),
						));
						object_builder.append(environment, key, value);
					}
				}
			}
		}
	}

	object_builder.build_object()
}

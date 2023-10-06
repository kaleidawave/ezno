// Type -> Object using internal

use checker::{types::TypeStore, PostCheckData, SpecialExpressions, Type, TypeId};
use parser::{
	property_key::AlwaysPublic, visiting::VisitorMut, ASTNode, Expression, PropertyKey, Span,
};

pub struct RuntimeTypeCompiler;

impl VisitorMut<Expression, PostCheckData<parser::Module>> for RuntimeTypeCompiler {
	fn visit_mut(
		&mut self,
		item: &mut Expression,
		data: &mut PostCheckData<parser::Module>,
		_chain: &parser::visiting::Chain,
	) {
		let point = item.get_position();
		let get = data.type_mappings.special_expressions.get(point.start);
		if let Some(special) = get {
			match special {
				SpecialExpressions::CompileOut => {
					let ty = data
						.type_mappings
						.expressions_to_instances
						.get_exact(point.clone())
						.unwrap();
					*item = type_to_runtime_expression(ty.get_value(), point.clone(), data);
				}
				SpecialExpressions::Marker => {
					// TODO argument might have side effect !
					*item = Expression::Null(point.clone());
				}
			}
		}
	}
}

fn type_to_runtime_expression(
	ty: TypeId,
	position: Span,
	data: &mut PostCheckData<parser::Module>,
) -> Expression {
	fn type_to_property_key(key: TypeId, types: &TypeStore) -> PropertyKey<AlwaysPublic> {
		if let Type::Constant(cst) = types.get_type_by_id(key) {
			match cst {
				checker::Constant::Number(number) => PropertyKey::NumberLiteral(
					parser::NumberStructure::Number(number.clone().into()),
					Span::NULL_SPAN,
				),
				checker::Constant::String(string) => {
					PropertyKey::StringLiteral(string.clone(), Span::NULL_SPAN)
				}
				checker::Constant::Boolean(_) => todo!(),
				checker::Constant::Regexp(_) => todo!(),
				checker::Constant::Symbol { .. } => todo!(),
				checker::Constant::Undefined => todo!(),
				checker::Constant::Null => todo!(),
				checker::Constant::NaN => todo!(),
			}
		} else {
			unreachable!()
		}
	}

	if let Type::Constant(cst) = data.types.get_type_by_id(ty) {
		match cst {
			checker::Constant::Number(number) => Expression::NumberLiteral(
				parser::NumberStructure::Number(number.clone().into()),
				position,
			),
			checker::Constant::String(string) => {
				Expression::StringLiteral(string.clone(), parser::Quoted::Double, position)
			}
			checker::Constant::Boolean(value) => Expression::BooleanLiteral(*value, position),
			checker::Constant::Regexp(_) => todo!(),
			checker::Constant::Symbol { .. } => todo!(),
			checker::Constant::Undefined => {
				Expression::VariableReference("undefined".to_owned(), position)
			}
			checker::Constant::Null => Expression::Null(position),
			checker::Constant::NaN => todo!(),
		}
	} else {
		let members = data
			.root
			.get_properties_on_type(ty)
			.into_iter()
			.map(|(key, value)| {
				let property_key = type_to_property_key(key, &data.types);
				parser::expressions::object_literal::ObjectLiteralMember::Property(
					property_key.into(),
					type_to_runtime_expression(value, position.clone(), data),
					position.clone(),
				)
			})
			.collect();

		Expression::ObjectLiteral(parser::expressions::object_literal::ObjectLiteral {
			members,
			position,
		})
	}
}

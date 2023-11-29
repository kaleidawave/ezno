use std::marker::PhantomData;

use source_map::{Span, SpanWithSource};

use crate::{
	behavior::objects::ObjectBuilder,
	types::{calling::CallingInput, cast_as_string, SynthesisedArgument},
	CheckingData, Constant, Environment, Instance, Type, TypeId,
};

pub enum TemplateLiteralPart<'a, T> {
	Static(&'a str),
	Dynamic(&'a T),
}

pub fn synthesise_template_literal<'a, T, M>(
	tag: Option<TypeId>,
	mut parts_iter: impl Iterator<Item = TemplateLiteralPart<'a, M::Expression>> + 'a,
	position: &Span,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, M>,
) -> TypeId
where
	T: crate::ReadFromFS,
	M: crate::ASTImplementation,
	M::Expression: 'a,
{
	fn part_to_type<T: crate::ReadFromFS, M: crate::ASTImplementation>(
		first: &TemplateLiteralPart<M::Expression>,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> crate::TypeId {
		match first {
			TemplateLiteralPart::Static(static_part) => {
				checking_data.types.new_constant_type(Constant::String((*static_part).to_owned()))
			}
			TemplateLiteralPart::Dynamic(expression) => {
				// TODO tidy
				let value = M::synthesise_expression(
					expression,
					TypeId::ANY_TYPE,
					environment,
					checking_data,
				);
				if let Type::Constant(cst) = checking_data.types.get_type_by_id(value) {
					let value = cast_as_string(cst, checking_data.options.strict_casts).unwrap();
					checking_data.types.new_constant_type(Constant::String(value))
				} else {
					crate::utils::notify!("Need to cast to string...");
					value
				}
			}
		}
	}

	if let Some(tag) = tag {
		// TODO use tuple type
		let mut static_parts = ObjectBuilder::new(
			Some(TypeId::ARRAY_TYPE),
			&mut checking_data.types,
			&mut environment.facts,
		);

		// TODO position
		let mut arguments = Vec::<SynthesisedArgument>::new();
		let mut static_part_count = 0u16;
		for part in parts_iter {
			match part {
				p @ TemplateLiteralPart::Static(_) => {
					let value = part_to_type(&p, environment, checking_data);
					static_parts.append(
						environment,
						crate::context::facts::Publicity::Public,
						crate::types::properties::PropertyKey::from_usize(static_part_count.into()),
						crate::PropertyValue::Value(value),
						// TODO should static parts should have position?
						None,
					);
					static_part_count += 1;
				}
				p @ TemplateLiteralPart::Dynamic(_) => {
					let ty = part_to_type(&p, environment, checking_data);
					arguments.push(SynthesisedArgument::NonSpread {
						ty,
						// TODO position
						position: SpanWithSource::NULL_SPAN,
					});
				}
			}
		}

		arguments.insert(
			0,
			SynthesisedArgument::NonSpread {
				ty: static_parts.build_object(),
				// TODO position
				position: SpanWithSource::NULL_SPAN,
			},
		);

		let call_site = position.clone().with_source(environment.get_source());
		crate::types::calling::call_type_handle_errors(
			tag,
			CallingInput {
				called_with_new: crate::types::calling::CalledWithNew::None,
				this_value: crate::behavior::functions::ThisValue::UseParent,
				call_site,
				call_site_type_arguments: None,
			},
			environment,
			arguments,
			checking_data,
		)
		.0
	} else {
		// Bit weird but makes Rust happy
		if let Some(first) = parts_iter.next() {
			let mut acc = part_to_type(&first, environment, checking_data);
			for rest in parts_iter {
				let other = part_to_type(&rest, environment, checking_data);
				let result = super::operations::evaluate_mathematical_operation(
					acc,
					crate::behavior::operations::MathematicalAndBitwise::Add,
					other,
					&mut checking_data.types,
					checking_data.options.strict_casts,
				);
				match result {
					Ok(result) => acc = result,
					Err(()) => {
						crate::utils::notify!("Invalid template literal concatenation");
					}
				}
			}
			acc
		} else {
			checking_data.types.new_constant_type(Constant::String(String::new()))
		}
	}
}

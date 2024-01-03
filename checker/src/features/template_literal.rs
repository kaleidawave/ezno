use source_map::{Span, SpanWithSource};

use crate::{
	context::invocation::CheckThings,
	features::objects::ObjectBuilder,
	types::{calling::CallingInput, cast_as_string, SynthesisedArgument},
	CheckingData, Constant, Environment, Type, TypeId,
};

#[derive(Copy, Clone)]
pub enum TemplateLiteralPart<'a, T> {
	Static(&'a str),
	Dynamic(&'a T),
}

#[allow(clippy::needless_pass_by_value)]
pub fn synthesise_template_literal<'a, T, A>(
	tag: Option<TypeId>,
	mut parts_iter: impl Iterator<Item = TemplateLiteralPart<'a, A::Expression<'a>>> + 'a,
	position: &Span,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> TypeId
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
	A::Expression<'a>: 'a,
{
	#[allow(clippy::needless_pass_by_value)]
	fn part_to_type<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
		first: TemplateLiteralPart<'a, A::Expression<'a>>,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> crate::TypeId {
		match first {
			TemplateLiteralPart::Static(static_part) => {
				checking_data.types.new_constant_type(Constant::String((*static_part).to_owned()))
			}
			TemplateLiteralPart::Dynamic(expression) => {
				// TODO tidy
				let value = A::synthesise_expression(
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
					let value = part_to_type(p, environment, checking_data);
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
					arguments.push(SynthesisedArgument {
						value: part_to_type(p, environment, checking_data),
						// TODO position
						position: SpanWithSource::NULL_SPAN,
						spread: false,
					});
				}
			}
		}

		arguments.insert(
			0,
			SynthesisedArgument {
				value: static_parts.build_object(),
				// TODO position
				position: SpanWithSource::NULL_SPAN,
				spread: false,
			},
		);

		let call_site = position.with_source(environment.get_source());
		match crate::types::calling::call_type(
			tag,
			arguments,
			CallingInput {
				called_with_new: crate::types::calling::CalledWithNew::None,
				this_value: crate::features::functions::ThisValue::UseParent,
				call_site,
				call_site_type_arguments: None,
			},
			environment,
			&mut CheckThings,
			&mut checking_data.types,
		) {
			Ok(res) => res.returned_type,
			Err(_) => {
				todo!("JSX Calling error")
			}
		}
	} else {
		// Bit weird but makes Rust happy
		if let Some(first) = parts_iter.next() {
			let mut acc = part_to_type(first, environment, checking_data);
			for rest in parts_iter {
				let other = part_to_type(rest, environment, checking_data);
				let result = super::operations::evaluate_mathematical_operation(
					acc,
					crate::features::operations::MathematicalAndBitwise::Add,
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

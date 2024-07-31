use source_map::SpanWithSource;

use crate::{
	context::invocation::CheckThings,
	diagnostics::TypeCheckError,
	features::objects::ObjectBuilder,
	types::{
		calling::{application_result_to_return_type, Callable, CallingContext, CallingInput},
		cast_as_string, SynthesisedArgument, TypeStore,
	},
	CheckingData, Constant, Environment, Type, TypeId,
};

#[derive(Copy, Clone)]
pub enum TemplateLiteralPart<'a, T> {
	Static(&'a str),
	Dynamic(&'a T),
}

#[allow(clippy::needless_pass_by_value)]
pub fn synthesise_template_literal_expression<'a, T, A>(
	tag: Option<TypeId>,
	mut parts_iter: impl Iterator<Item = TemplateLiteralPart<'a, A::Expression<'a>>> + 'a,
	position: SpanWithSource,
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
					crate::utilities::notify!("Need to cast to string...");
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
			position,
			&mut environment.info,
		);

		// TODO position
		let mut arguments = Vec::<SynthesisedArgument>::new();
		let mut static_part_count = 0u16;
		for part in parts_iter {
			match part {
				p @ TemplateLiteralPart::Static(_) => {
					let value = part_to_type(p, environment, checking_data);
					static_parts.append(
						crate::types::properties::Publicity::Public,
						crate::types::properties::PropertyKey::from_usize(static_part_count.into()),
						crate::PropertyValue::Value(value),
						// TODO should static parts should have position?
						position,
						&mut environment.info,
					);
					static_part_count += 1;
				}
				TemplateLiteralPart::Dynamic(expression) => {
					let position =
						A::expression_position(expression).with_source(environment.get_source());
					arguments.push(SynthesisedArgument {
						value: part_to_type(
							TemplateLiteralPart::Dynamic(expression),
							environment,
							checking_data,
						),
						position,
						spread: false,
					});
				}
			}
		}

		{
			// TODO spread
			let length = checking_data.types.new_constant_type(Constant::Number(
				f64::from(static_part_count).try_into().unwrap(),
			));

			// TODO: Should there be a position here?
			static_parts.append(
				crate::types::properties::Publicity::Public,
				crate::types::properties::PropertyKey::String("length".into()),
				crate::types::properties::PropertyValue::Value(length),
				position,
				&mut environment.info,
			);
		}

		arguments.insert(
			0,
			SynthesisedArgument {
				value: static_parts.build_object(),
				// TODO position
				position: source_map::Nullable::NULL,
				spread: false,
			},
		);

		let mut check_things = CheckThings { debug_types: checking_data.options.debug_types };

		let input = CallingInput {
			called_with_new: crate::types::calling::CalledWithNew::None,
			call_site: position,
			max_inline: checking_data.options.max_inline_count,
		};
		let mut diagnostics = Default::default();
		let result = Callable::Type(tag).call(
			arguments,
			input,
			environment,
			(&mut check_things, &mut diagnostics),
			&mut checking_data.types,
		);
		diagnostics
			.append_to(CallingContext::TemplateLiteral, &mut checking_data.diagnostics_container);
		match result {
			Ok(res) => {
				application_result_to_return_type(res.result, environment, &mut checking_data.types)
			}
			Err(error) => error.returned_type,
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
						crate::utilities::notify!("Invalid template literal concatenation");
					}
				}
			}
			acc
		} else {
			checking_data.types.new_constant_type(Constant::String(String::new()))
		}
	}
}

/// **Expects static part first**
///
/// TODO API is different to the `synthesise_template_literal_expression` above
pub fn synthesize_template_literal_type(parts: Vec<TypeId>, types: &mut TypeStore) -> TypeId {
	let mut parts_iter = parts.into_iter();
	if let Some(first) = parts_iter.next() {
		let mut acc = first;
		for other in parts_iter {
			// TODO unfold_alias function
			let other = if let Type::AliasTo { to, .. } = types.get_type_by_id(other) {
				*to
			} else {
				other
			};
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalAndBitwise::Add,
				other,
				types,
				true,
			);
			match result {
				Ok(result) => acc = result,
				Err(()) => {
					// crate::utilities::notify!(
					// 	"acc is {:?}, other is {:?}",
					// 	types.get_type_by_id(acc),
					// 	types.get_type_by_id(other)
					// );
					crate::utilities::notify!("Invalid type template literal concatenation");
				}
			}
		}
		acc
	} else {
		types.new_constant_type(Constant::String(String::new()))
	}
}

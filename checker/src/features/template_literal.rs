use source_map::SpanWithSource;

use crate::{
	context::invocation::CheckThings,
	features::objects::ObjectBuilder,
	types::{
		calling::{
			application_result_to_return_type, Callable, CallingContext, CallingInput,
			SynthesisedArgument,
		},
		cast_as_string, TypeStore,
	},
	CheckingData, Constant, Environment, Type, TypeId,
};

#[allow(clippy::needless_pass_by_value)]
pub fn synthesise_template_literal_expression<'a, T, A>(
	tag: Option<TypeId>,
	parts_iter: impl Iterator<Item = (&'a str, &'a A::MultipleExpression<'a>)> + 'a,
	final_part: &'a str,
	position: SpanWithSource,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> TypeId
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
	A::MultipleExpression<'a>: 'a,
{
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
		for (static_part, dynamic_part) in parts_iter {
			{
				let value =
					checking_data.types.new_constant_type(Constant::String(static_part.to_owned()));
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
			{
				let value = A::synthesise_multiple_expression(
					dynamic_part,
					TypeId::ANY_TYPE,
					environment,
					checking_data,
				);
				let value = if let Type::Constant(cst) = checking_data.types.get_type_by_id(value) {
					let value = cast_as_string(cst, checking_data.options.strict_casts).unwrap();
					checking_data.types.new_constant_type(Constant::String(value))
				} else {
					crate::utilities::notify!("Need to cast to string...");
					value
				};
				let position = A::multiple_expression_position(dynamic_part)
					.with_source(environment.get_source());
				arguments.push(SynthesisedArgument { value, position, spread: false });
			}
		}

		if !final_part.is_empty() {
			let value =
				checking_data.types.new_constant_type(Constant::String(final_part.to_owned()));
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

		{
			// TODO spread
			let static_part_array_length = checking_data.types.new_constant_type(Constant::Number(
				f64::from(static_part_count).try_into().unwrap(),
			));

			// TODO: Should there be a position here?
			static_parts.append(
				crate::types::properties::Publicity::Public,
				crate::types::properties::PropertyKey::String(std::borrow::Cow::Borrowed("length")),
				crate::types::properties::PropertyValue::Value(static_part_array_length),
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
		let mut acc = TypeId::EMPTY_STRING;
		for (static_part, dynamic_part) in parts_iter {
			let lhs =
				checking_data.types.new_constant_type(Constant::String(static_part.to_owned()));
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalAndBitwise::Add,
				lhs,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			);
			if let Ok(result) = result {
				acc = result;
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				return TypeId::ERROR_TYPE;
			}
			let rhs = A::synthesise_multiple_expression(
				dynamic_part,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalAndBitwise::Add,
				rhs,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			);
			if let Ok(result) = result {
				acc = result;
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				return TypeId::ERROR_TYPE;
			}
		}
		if final_part.is_empty() {
			acc
		} else {
			let value =
				checking_data.types.new_constant_type(Constant::String(final_part.to_owned()));
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalAndBitwise::Add,
				value,
				&mut checking_data.types,
				checking_data.options.strict_casts,
			);
			if let Ok(result) = result {
				result
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				TypeId::ERROR_TYPE
			}
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

use source_map::SpanWithSource;
use std::borrow::Cow;

use crate::{
	context::invocation::CheckThings,
	features::objects::ObjectBuilder,
	types::{
		calling::{
			application_result_to_return_type, Callable, CallingContext, CallingInput,
			SynthesisedArgument,
		},
		cast_as_string,
	},
	CheckingData, Constant, Environment, Type, TypeId,
};

/// Assumes that the text parts have been unesscaped
#[allow(clippy::needless_pass_by_value)]
pub fn synthesise_template_literal_expression<'a, T, A>(
	tag: Option<TypeId>,
	parts_iter: impl Iterator<Item = (Cow<'a, str>, &'a A::Expression<'a>)> + 'a,
	final_part: Cow<'a, str>,
	position: SpanWithSource,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> TypeId
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
	A::Expression<'a>: 'a,
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
				let value = checking_data
					.types
					.new_constant_type(Constant::String(static_part.into_owned()));
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
				let value = A::synthesise_expression(
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
				let position =
					A::expression_position(dynamic_part).with_source(environment.get_source());
				arguments.push(SynthesisedArgument { value, position, spread: false });
			}
		}

		if !final_part.is_empty() {
			let value =
				checking_data.types.new_constant_type(Constant::String(final_part.into_owned()));
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
			let static_part_array_length = checking_data
				.types
				.new_constant_type(Constant::Number(f64::from(static_part_count)));

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
				checking_data.types.new_constant_type(Constant::String(static_part.into_owned()));
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalOrBitwiseOperation::Add,
				lhs,
				environment,
				&mut checking_data.types,
				checking_data.options.strict_casts,
				checking_data.options.advanced_numbers,
			);
			if let Ok(result) = result {
				acc = result;
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				return TypeId::UNIMPLEMENTED_ERROR_TYPE;
			}
			let rhs = A::synthesise_expression(
				dynamic_part,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalOrBitwiseOperation::Add,
				rhs,
				environment,
				&mut checking_data.types,
				checking_data.options.strict_casts,
				checking_data.options.advanced_numbers,
			);
			if let Ok(result) = result {
				acc = result;
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				return TypeId::UNIMPLEMENTED_ERROR_TYPE;
			}
		}
		if final_part.is_empty() {
			acc
		} else {
			let value =
				checking_data.types.new_constant_type(Constant::String(final_part.into_owned()));
			let result = super::operations::evaluate_mathematical_operation(
				acc,
				crate::features::operations::MathematicalOrBitwiseOperation::Add,
				value,
				environment,
				&mut checking_data.types,
				checking_data.options.strict_casts,
				checking_data.options.advanced_numbers,
			);
			if let Ok(result) = result {
				result
			} else {
				crate::utilities::notify!("Invalid template literal concatenation");
				TypeId::UNIMPLEMENTED_ERROR_TYPE
			}
		}
	}
}

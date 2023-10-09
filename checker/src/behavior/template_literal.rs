use std::marker::PhantomData;

use source_map::{Span, SpanWithSource};

use crate::{
	behavior::objects::ObjectBuilder,
	types::{cast_as_string, SynthesisedArgument},
	CheckingData, Constant, Environment, Instance, SynthesisableExpression, Type, TypeId,
};

pub enum TemplateLiteralPart<'a, M: crate::SynthesisableModule, TExpr: SynthesisableExpression<M>> {
	Static(&'a str),
	Dynamic(&'a TExpr, PhantomData<M>),
}

pub fn synthesise_template_literal<'a, T, M, TExpr>(
	tag: Option<TypeId>,
	mut parts_iter: impl Iterator<Item = TemplateLiteralPart<'a, M, TExpr>> + 'a,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, M>,
) -> Instance
where
	T: crate::ReadFromFS,
	M: crate::SynthesisableModule,
	TExpr: SynthesisableExpression<M> + 'a,
{
	fn part_to_type<
		T: crate::ReadFromFS,
		M: crate::SynthesisableModule,
		TExpr: SynthesisableExpression<M>,
	>(
		first: TemplateLiteralPart<M, TExpr>,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> crate::TypeId {
		match first {
			TemplateLiteralPart::Static(static_part) => {
				checking_data.types.new_constant_type(Constant::String(static_part.to_owned()))
			}
			TemplateLiteralPart::Dynamic(expression, _) => {
				// TODO tidy
				let value = SynthesisableExpression::synthesise_expression(
					expression,
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
		let mut static_parts = ObjectBuilder::new(
			Some(TypeId::ARRAY_TYPE),
			&mut checking_data.types,
			&mut environment.facts,
		);

		// TODO position
		let mut arguments = Vec::<SynthesisedArgument>::new();

		arguments.insert(
			0,
			SynthesisedArgument::NonSpread {
				ty: static_parts.build_object(),
				position: SpanWithSource::NULL_SPAN,
			},
		);

		// TODO make static parts immutable

		todo!();

	// crate::types::calling::call_type(
	// 	tag,
	// 	crate::events::CalledWithNew::None,
	// 	None,
	// 	None,
	// 	arguments,
	// 	call_site,
	// 	environment,
	// 	&mut checking_data.types,
	// );
	} else if let Some(first) = parts_iter.next() {
		let mut acc = part_to_type(first, environment, checking_data);
		for rest in parts_iter {
			let other = part_to_type(rest, environment, checking_data);
			acc = super::operations::evaluate_mathematical_operation(
				acc,
				crate::behavior::operations::MathematicalAndBitwise::Add,
				other,
				&mut checking_data.types,
			)
			.unwrap()
		}
		Instance::RValue(acc)
	} else {
		Instance::RValue(checking_data.types.new_constant_type(Constant::String("".into())))
	}
}

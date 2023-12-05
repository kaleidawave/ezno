use crate::{
	behavior::operations::CanonicalEqualityAndInequality,
	events::{apply_event, Event, RootReference},
	types::{poly_types::FunctionTypeArguments, Constructor, PolyNature},
	CheckingData, Constant, Environment, Scope, Type, TypeId,
};

pub enum LoopBehavior<'a, A: crate::ASTImplementation> {
	While(&'a A::MultipleExpression<'a>),
	DoWhile(&'a A::MultipleExpression<'a>),
	// For { initializer: TypeId, condition: A::Expression, afterthought: A::Expression },
}

/// TODO expand to `for (let ...` and `for (of|in)`
pub fn evaluate_loop<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
	behavior: LoopBehavior<'a, A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	loop_body: impl FnOnce(&mut Environment, &mut CheckingData<T, A>),
) {
	let (res, events, ..) = environment.new_lexical_environment_fold_into_parent(
		Scope::Looping {},
		checking_data,
		|environment, checking_data| {
			let condition = match behavior {
				LoopBehavior::While(expression) => {
					let condition = A::synthesise_multiple_expression(
						expression,
						TypeId::ANY_TYPE,
						environment,
						checking_data,
					);

					environment.facts.events.push(Event::Conditionally {
						condition,
						events_if_truthy: Box::new([Event::Break { position: None }]),
						else_events: Default::default(),
						position: None,
					});

					loop_body(environment, checking_data);

					condition
				}
				LoopBehavior::DoWhile(expression) => {
					loop_body(environment, checking_data);

					let condition = A::synthesise_multiple_expression(
						expression,
						TypeId::ANY_TYPE,
						environment,
						checking_data,
					);

					environment.facts.events.push(Event::Conditionally {
						condition,
						events_if_truthy: Box::new([Event::Break { position: None }]),
						else_events: Default::default(),
						position: None,
					});

					condition
				}
			};

			let condition_ty = checking_data.types.get_type_by_id(condition);

			// TODO and for less than equal
			if let Type::Constructor(Constructor::CanonicalRelationOperator {
				lhs,
				operator: CanonicalEqualityAndInequality::LessThan,
				rhs,
			}) = condition_ty
			{
				// TODO sort by constant

				let variable =
					if let Type::RootPolyType(PolyNature::FreeVariable { reference, based_on }) =
						checking_data.types.get_type_by_id(*lhs)
					{
						Some(reference.clone())
					} else {
						None
					};

				let condition = if let Type::Constant(Constant::Number(n)) =
					checking_data.types.get_type_by_id(*rhs)
				{
					Some(*n)
				} else {
					None
				};

				let increment = variable.as_ref().and_then(|v| {
					if let RootReference::Variable(v) = v {
						let ty = checking_data.types.get_type_by_id(
							*environment.facts.variable_current_value.get(&v).unwrap(),
						);
						if let Type::Constructor(Constructor::BinaryOperator {
							lhs: assignment,
							operator,
							rhs,
						}) = ty
						{
							crate::utils::notify!("{:?}, equals {:?}", lhs, lhs == assignment);
							if let Type::Constant(Constant::Number(n)) =
								checking_data.types.get_type_by_id(*rhs)
							{
								Some(*n)
							} else {
								None
							}
						} else {
							None
						}
					} else {
						None
					}
				});

				Some((variable, increment, condition))
			} else {
				None
			}
		},
	);
	let events = events.unwrap().0;

	crate::utils::notify!("events = {:?}", events);

	if let Some((Some(RootReference::Variable(reference)), Some(increment), Some(end))) = res {
		let current_ty = checking_data
			.types
			.get_type_by_id(*environment.facts.variable_current_value.get(&reference).unwrap());

		if let crate::Type::Constant(Constant::Number(s)) = current_ty {
			crate::utils::notify!("start {}, end {}, increment {}", *s, end, increment);
			let loops = (end - *s) / increment;
			crate::utils::notify!("Loops = {}", loops);
			for _ in 0..(loops.into_inner() as usize) {
				let mut arguments = FunctionTypeArguments {
					structure_arguments: Default::default(),
					local_arguments: map_vec::Map::new(),
					closure_id: Default::default(),
				};
				// TODO temp skip
				// TODO can't find free variable id...?
				for event in events.clone().into_iter() {
					// TODO temp
					apply_event(
						event,
						crate::behavior::functions::ThisValue::UseParent,
						&mut arguments,
						environment,
						&mut crate::context::calling::Target::new_default(),
						&mut checking_data.types,
					);
				}
			}
		} else {
			todo!()
		}
	} else {
		todo!("dependence everywhere")
	}
}

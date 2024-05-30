//! Exception checking is enabled by events

use crate::{
	context::VariableRegisterArguments,
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	events::{Event, FinalEvent, Trapped},
	subtyping::type_is_subtype_object,
	CheckingData, Environment, Scope, Type, TypeId,
};

/// TODO
/// - `finally`
pub fn new_try_context<'a, T: crate::ReadFromFS, A: crate::ASTImplementation>(
	try_block: &'a A::Block<'a>,
	catch_block: Option<(
		&'a A::Block<'a>,
		Option<(&'a A::VariableField<'a>, Option<&'a A::TypeAnnotation<'a>>)>,
	)>,
	_finally_block: Option<&'a A::Block<'a>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	let (thrown_type, mut throw_events) = {
		let mut try_block_environment = environment.new_lexical_environment(Scope::TryBlock {});

		A::synthesise_block(&try_block, &mut try_block_environment, checking_data);
		crate::utilities::notify!("TODO also get possible impure functions");

		let mut thrown_type_acc = TypeId::NEVER_TYPE;

		// TODO should events be removed?
		for event in &try_block_environment.info.events {
			// TODO possible getters
			if let Event::CallsType { possibly_thrown, .. } = event {
				if let Some(thrown) = possibly_thrown {
					thrown_type_acc = checking_data.types.new_or_type(thrown_type_acc, *thrown);
				} else {
					crate::utilities::notify!("Maybe not never if not pure");
				}
			} else if let Event::Conditionally { .. } = event {
				crate::utilities::notify!("TODO");
			} else if let Event::Iterate { .. } = event {
				crate::utilities::notify!("TODO");
			} else if let Event::FinalEvent(FinalEvent::Throw { thrown, position: _ }) = event {
				thrown_type_acc = checking_data.types.new_or_type(thrown_type_acc, *thrown);
			}
		}

		// TODO want to also get whether condition. Any unknowns etc

		// TODO merge info stuff
		let events = try_block_environment.info.events;

		(thrown_type_acc, events)
	};

	if let Some((catch_block, exception_variable)) = catch_block {
		if thrown_type == TypeId::NEVER_TYPE {
			crate::utilities::notify!("warning");
		}

		let mut catch_block_environment = environment.new_lexical_environment(Scope::CatchBlock {});

		// Important that this is declared in the same one as the block
		let constraint = if let Some((clause, ty_annotation)) = exception_variable {
			let catch_variable_type = if let Some(ty_annotation) = ty_annotation {
				let catch_type_id = A::synthesise_type_annotation(
					ty_annotation,
					&mut catch_block_environment,
					checking_data,
				);
				check_catch_type(
					ty_annotation,
					catch_type_id,
					thrown_type,
					&mut catch_block_environment,
					checking_data,
				);
				Some(catch_type_id)
			} else {
				None
			};

			// TODO only conditional
			let throw_variable_type = checking_data.types.register_type(Type::RootPolyType(
				crate::types::PolyNature::CatchVariable(
					catch_variable_type.unwrap_or(TypeId::ANY_TYPE),
				),
			));

			let arguments = VariableRegisterArguments {
				// TODO catch variable constant option
				constant: true,
				space: catch_variable_type,
				initial_value: Some(throw_variable_type),
			};

			A::declare_and_assign_to_fields(
				clause,
				&mut catch_block_environment,
				checking_data,
				arguments,
			);

			Some(Trapped { constrained: catch_variable_type, generic_type: throw_variable_type })
		} else {
			None
		};

		A::synthesise_block(&catch_block, &mut catch_block_environment, checking_data);

		let mut catch_events = catch_block_environment.info.events;

		let try_event = Event::ExceptionTrap {
			investigate: throw_events.len() as u32,
			handle: catch_events.len() as u32,
			finally: 0,
			trapped_type_id: constraint,
		};

		environment.info.events.push(try_event);
		environment.info.events.append(&mut throw_events);
		environment.info.events.append(&mut catch_events);
	} else {
		// TODO finally
	}

	// environment
}

fn check_catch_type<'a, T, A: crate::ASTImplementation>(
	catch_annotation: &'a A::TypeAnnotation<'a>,
	catch_type: TypeId,
	thrown_type: TypeId,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	let result =
		type_is_subtype_object(catch_type, thrown_type, environment, &mut checking_data.types);

	if let crate::subtyping::SubTypeResult::IsNotSubType(_) = result {
		let expected = TypeStringRepresentation::from_type_id(
			thrown_type,
			environment,
			&checking_data.types,
			false,
		);
		let found = TypeStringRepresentation::from_type_id(
			catch_type,
			environment,
			&checking_data.types,
			false,
		);

		let at =
			A::type_annotation_position(catch_annotation).with_source(environment.get_source());

		checking_data.diagnostics_container.add_error(TypeCheckError::CatchTypeDoesNotMatch {
			at,
			expected,
			found,
		});
	}
}

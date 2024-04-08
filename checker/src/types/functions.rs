/// Contains definitions of structures around functions
use std::collections::HashMap;

use source_map::{BaseSpan, Nullable, SpanWithSource};

use crate::{
	call_type_handle_errors,
	context::environment::FunctionScope,
	events::{Event, RootReference},
	features::functions::{ClassPropertiesToRegister, ClosedOverVariables, FunctionBehavior},
	types::calling::CallingInput,
	CheckingData, Environment, FunctionId, GenericTypeParameters, LocalInformation, Scope, Type,
	TypeId,
};

use super::{classes::register_properties_into_environment, TypeStore};

/// This is a mesh of annotation and actually defined functions
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct FunctionType {
	/// Syntax defined pointer
	pub id: FunctionId,
	/// TODO unsure about this field and how it tails with Pi Types
	pub type_parameters: Option<GenericTypeParameters>,
	pub parameters: SynthesisedParameters,
	/// This is just aesthetic TODO also throw
	pub return_type: TypeId,
	/// If async, generator and what to do with `this`
	pub behavior: FunctionBehavior,
	/// See [`FunctionEffect`]
	pub effect: FunctionEffect,
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionEffect {
	SideEffects {
		/// Note that a function can still be considered to be 'pure' and have a non-empty vector of events
		events: Vec<Event>,

		/// Things that this function pulls in. Converse of closed over which is where results below use
		/// variables in this scope.
		free_variables: HashMap<RootReference, TypeId>,

		/// References it needs to retain for returning / other effects where things go out.
		///
		/// The type is the initial value of the closure variable when this is called
		closed_over_variables: ClosedOverVariables,
	},
	Constant(String),
	InputOutput(String),
	Unknown,
}

#[derive(Debug)]
pub enum InternalFunctionEffect {
	Constant(String),
	InputOutput(String),
}

impl From<InternalFunctionEffect> for FunctionEffect {
	fn from(value: InternalFunctionEffect) -> Self {
		match value {
			InternalFunctionEffect::Constant(identifier) => FunctionEffect::Constant(identifier),
			InternalFunctionEffect::InputOutput(identifier) => {
				FunctionEffect::InputOutput(identifier)
			}
		}
	}
}

impl FunctionType {
	pub(crate) fn new_auto_constructor<T: crate::ReadFromFS, A: crate::ASTImplementation>(
		class_prototype: TypeId,
		extends: Option<TypeId>,
		properties: ClassPropertiesToRegister<A>,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Self {
		let scope = Scope::Function(FunctionScope::Constructor {
			extends: false,
			type_of_super: None,
			// Set later
			this_object_type: TypeId::ERROR_TYPE,
		});

		let (on, env_data, _) = environment.new_lexical_environment_fold_into_parent(
			scope,
			checking_data,
			|environment, checking_data| {
				let on = create_this_before_function_synthesis(
					&mut checking_data.types,
					&mut environment.info,
					class_prototype,
				);
				if let Scope::Function(FunctionScope::Constructor {
					ref mut this_object_type,
					..
				}) = environment.context_type.scope
				{
					*this_object_type = on;
				}

				if let Some(extends) = extends {
					crate::utils::notify!("Here extends");
					let called_with_new =
						super::calling::CalledWithNew::SpecialSuperCall { this_type: on };

					let input = CallingInput {
						call_site: BaseSpan::NULL,
						called_with_new,
						call_site_type_arguments: None,
					};
					let _ = call_type_handle_errors(
						extends,
						&[],
						input,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
				}

				register_properties_into_environment(environment, on, checking_data, properties);

				on
			},
		);

		let behavior =
			FunctionBehavior::Constructor { non_super_prototype: None, this_object_type: on };

		let (info, _free_variables) = env_data.unwrap();
		Self {
			id: crate::FunctionId::AUTO_CONSTRUCTOR,
			type_parameters: None,
			parameters: SynthesisedParameters::default(),
			return_type: on,
			behavior,
			effect: FunctionEffect::SideEffects {
				events: info.events,
				free_variables: Default::default(),
				closed_over_variables: Default::default(),
			},
		}
	}
}

/// For inside the function
pub(crate) fn create_this_before_function_synthesis(
	types: &mut TypeStore,
	info: &mut LocalInformation,
	prototype: TypeId,
) -> TypeId {
	let ty = types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

	// crate::utils::notify!("Registered 'this' in constructor as {:?}", ty);

	let value = Event::CreateObject {
		referenced_in_scope_as: ty,
		prototype: crate::events::PrototypeArgument::Yeah(prototype),
		position: None,
		// TODO right?
		is_function_this: true,
	};
	info.events.push(value);

	ty
}

/// TODO temp
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum GetSet {
	Get,
	Set,
}

/// Optionality is indicated by what vector it is in [`SynthesisedParameters`]
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedParameter {
	pub name: String,
	/// This is also for parameters with default (which is handled behind the scenes)
	pub is_optional: bool,
	/// This is the generic parameter type, not the restriction
	pub ty: TypeId,
	pub position: SpanWithSource,
}

/// **Note that the [Type] here is not array like**
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedRestParameter {
	pub name: String,
	/// This is the item type, aka the `T`` of `Array<T>`
	pub item_type: TypeId,
	/// This is the generic type (to substitute into)
	pub ty: TypeId,
	pub position: SpanWithSource,
}

/// A type of a collection of function parameters
///
/// This applies for source functions
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedParameters {
	// Even though these vectors are the same type, the latter allows for elided arguments
	pub parameters: Vec<SynthesisedParameter>,
	pub rest_parameter: Option<SynthesisedRestParameter>,
}

impl SynthesisedParameters {
	// TODO should be aware of undefined in optionals possibly
	pub(crate) fn get_parameter_type_at_index(
		&self,
		idx: usize,
	) -> Option<(TypeId, SpanWithSource)> {
		if let Some(param) = self.parameters.get(idx) {
			Some((param.ty, param.position))
		} else {
			self.rest_parameter.as_ref().map(|rest| (rest.item_type, rest.position))
		}
	}
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedArgument {
	pub(crate) spread: bool,
	pub(crate) value: TypeId,
	pub(crate) position: SpanWithSource,
}

impl SynthesisedArgument {
	pub fn non_spread_type(&self) -> Result<TypeId, ()> {
		if self.spread {
			Err(())
		} else {
			Ok(self.value)
		}
	}
}

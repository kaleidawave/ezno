use std::collections::HashMap;

use source_map::{Span, SpanWithSource};

use crate::{
	behavior::functions::{ClassPropertiesToRegister, FunctionBehavior},
	context::{
		environment::{self, FunctionScope},
		facts::Publicity,
		Context, ContextType,
	},
	events::{Event, RootReference},
	CheckingData, Environment, Facts, FunctionId, GenericTypeParameters, PropertyValue, Scope,
	Type, TypeId,
};

use super::{classes::register_properties_into_environment, TypeStore};

/// This is a mesh of annotation and actually defined functions
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct FunctionType {
	/// Syntax defined pointer
	pub id: FunctionId,

	pub constant_function: Option<String>,

	/// If async, generator and what to do with `this`
	pub behavior: FunctionBehavior,

	/// TODO not sure about this field and how it tails with Pi Types
	pub type_parameters: Option<GenericTypeParameters>,
	pub parameters: SynthesisedParameters,
	/// This is just aesthetic TODO also throw
	pub return_type: TypeId,

	/// Side effects of the function
	pub effects: Vec<Event>,

	/// Things that this function pulls in. Converse of closed over which is where results below use
	/// variables in this scope.
	pub free_variables: HashMap<RootReference, TypeId>,

	/// References it needs to retain for returning / other effects where things go out.
	///
	/// The type is the initial value of the closure variable when this is called
	pub closed_over_variables: HashMap<RootReference, TypeId>,
}

impl FunctionType {
	pub(crate) fn new_auto_constructor<
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
		S: ContextType,
	>(
		class_prototype: TypeId,
		properties: ClassPropertiesToRegister<A>,
		// TODO S overkill
		context: &mut crate::context::Context<S>,
		checking_data: &mut CheckingData<T, A>,
	) -> Self {
		let scope = Scope::Function(FunctionScope::Constructor {
			extends: false,
			type_of_super: None,
			this_object_type: TypeId::ERROR_TYPE,
		});

		let (on, env_data, _) = context.new_lexical_environment_fold_into_parent(
			scope,
			checking_data,
			|environment, checking_data| {
				let on = create_this_before_function_synthesis(
					&mut checking_data.types,
					&mut environment.facts,
					class_prototype,
				);
				if let Scope::Function(FunctionScope::Constructor {
					ref mut this_object_type,
					..
				}) = environment.context_type.scope
				{
					*this_object_type = on;
				}

				register_properties_into_environment(environment, on, checking_data, properties);
				on
			},
		);
		// TODO think Some fine
		// TODO
		let behavior =
			FunctionBehavior::Constructor { non_super_prototype: None, this_object_type: on };

		let (events, free_variables) = env_data.unwrap();
		Self {
			id: crate::FunctionId::AUTO_CONSTRUCTOR,
			constant_function: None,
			type_parameters: None,
			parameters: SynthesisedParameters::default(),
			// Only needed for printing
			return_type: on,
			effects: events,
			behavior,
			// TODO ???
			free_variables: Default::default(),
			closed_over_variables: Default::default(),
		}
	}
}

/// For inside the function
pub(crate) fn create_this_before_function_synthesis(
	types: &mut TypeStore,
	facts: &mut Facts,
	prototype: TypeId,
) -> TypeId {
	let ty = types.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

	crate::utils::notify!("Registered 'this' type as {:?}", ty);
	let value = Event::CreateObject {
		referenced_in_scope_as: ty,
		prototype: crate::events::PrototypeArgument::Yeah(prototype),
		position: None,
	};
	facts.events.push(value);

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
	/// This is the generic parameter type, not the restriction
	pub ty: TypeId,
	pub position: SpanWithSource,
	/// For optional parameters this is [TypeId::UNDEFINED_TYPE] else some type
	pub missing_value: Option<TypeId>,
}

/// **Note that the [Type] here is not array like**
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedRestParameter {
	pub name: String,
	/// This is the T, of Array<T>
	pub item_type: TypeId,
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
	pub(crate) fn get_type_constraint_at_index(&self, idx: usize) -> Option<TypeId> {
		if let Some(param) = self.parameters.get(idx) {
			Some(param.ty)
		} else {
			self.rest_parameter.as_ref().map(|rest| rest.item_type)
		}
	}
}

/// TODO spread should of tuples should expand into `NonSpread`
/// TODO spread for non heterogenous things
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
#[non_exhaustive]
pub enum SynthesisedArgument {
	/// This is the get value of a argument
	NonSpread { ty: TypeId, position: SpanWithSource },
	// TODO
	// Spread(Instance),
}

impl SynthesisedArgument {
	pub(crate) fn get_position(&self) -> SpanWithSource {
		match self {
			SynthesisedArgument::NonSpread { ty: _, position } => *position,
		}
	}

	// TODO: Remove when error is added
	#[allow(clippy::unnecessary_wraps)]
	pub(crate) fn to_type(&self) -> Result<TypeId, ()> {
		match self {
			SynthesisedArgument::NonSpread { ty, position: _ } => Ok(*ty),
		}
	}
}

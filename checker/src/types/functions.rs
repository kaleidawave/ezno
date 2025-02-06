/// Contains definitions of structures around functions
use std::collections::HashMap;

use source_map::{BaseSpan, Nullable, SpanWithSource};

use super::calling::{Callable, CallingContext, CallingInput};
use crate::{
	context::{environment::FunctionScope, invocation::CheckSyntax},
	events::{Event, RootReference},
	features::functions::{ClassPropertiesToRegister, ClosedOverVariables},
	CheckingData, Environment, FunctionId, Scope, TypeId,
};

pub use crate::{features::functions::ReturnType, GenericTypeParameters};

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
	/// Has synthesised events
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
	Constant {
		/// used to pick which function to calling in `constant_functions`
		///
		/// in the future this may need to have a prefix
		identifier: String,
		may_throw: Option<TypeId>,
	},
	InputOutput {
		/// not used in the checker, but may be useful to other tools
		identifier: String,
		may_throw: Option<TypeId>,
	},
	/// Such as a callback
	Unknown,
}

#[derive(Debug)]
pub enum InternalFunctionEffect {
	Constant { identifier: String, may_throw: Option<TypeId> },
	InputOutput { identifier: String, may_throw: Option<TypeId> },
}

impl From<InternalFunctionEffect> for FunctionEffect {
	fn from(value: InternalFunctionEffect) -> Self {
		match value {
			InternalFunctionEffect::Constant { identifier, may_throw } => {
				FunctionEffect::Constant { identifier, may_throw }
			}
			InternalFunctionEffect::InputOutput { identifier, may_throw } => {
				FunctionEffect::InputOutput { identifier, may_throw }
			}
		}
	}
}

impl FunctionType {
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn new_auto_constructor<T: crate::ReadFromFS, A: crate::ASTImplementation>(
		function_id: FunctionId,
		class_prototype: TypeId,
		extends: Option<TypeId>,
		name: TypeId,
		properties: ClassPropertiesToRegister<A>,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
		position: SpanWithSource,
	) -> Self {
		let scope = Scope::Function(FunctionScope::Constructor {
			extends: false,
			type_of_super: None,
			// Set later
			this_object_type: TypeId::IS_ASSIGNED_VALUE_LATER,
		});

		let (on, env_data, _) = environment.new_lexical_environment_fold_into_parent(
			scope,
			checking_data,
			|environment, checking_data| {
				let on = checking_data.types.create_this_object();
				if let Scope::Function(FunctionScope::Constructor {
					ref mut this_object_type,
					..
				}) = environment.context_type.scope
				{
					*this_object_type = on;
				}

				if let Some(extends) = extends {
					crate::utilities::notify!("Here extends");
					let called_with_new = super::calling::CalledWithNew::Super { this_type: on };

					let input = CallingInput {
						call_site: BaseSpan::NULL,
						called_with_new,
						max_inline: checking_data.options.max_inline_count,
					};
					let mut check_syntax =
						CheckSyntax { debug_types: checking_data.options.debug_types };
					let result = Callable::Type(extends).call(
						Vec::new(),
						input,
						environment,
						(&mut check_syntax, &mut checking_data.resolver),
						&mut checking_data.types,
					);

					match result {
						Ok(_) => {}
						Err(_error) => {}
					}
				}

				crate::types::classes::register_properties_into_environment(
					environment,
					on,
					checking_data,
					properties,
					position,
				);

				on
			},
		);

		let behavior = FunctionBehavior::Constructor {
			prototype: class_prototype,
			this_object_type: on,
			name,
		};

		let (info, _free_variables) = env_data.unwrap();
		Self {
			id: function_id,
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

/// TODO different place
/// TODO maybe generic
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionBehavior {
	/// For arrow functions, cannot have `this` bound
	ArrowFunction {
		is_async: bool,
	},
	Method {
		free_this_id: TypeId,
		is_async: bool,
		is_generator: bool,
		name: TypeId,
	},
	/// Functions defined `function`. Extends above by allowing `new`
	Function {
		/// This points the general `this` object.
		/// When calling with:
		/// - `new`: an arguments should set with (`free_this_id`, *new object*)
		/// - regularly: bound argument, else parent `this` (I think)
		this_id: TypeId,
		/// The function type. [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new)
		prototype: TypeId,
		is_async: bool,
		/// Cannot be called with `new` if true
		is_generator: bool,
		/// This is to implement <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/name>
		name: TypeId,
	},
	/// Constructors, require new
	Constructor {
		/// The prototype of the base object
		prototype: TypeId,
		/// The id of the generic that needs to be pulled out
		this_object_type: TypeId,
		name: TypeId,
	},
}

impl FunctionBehavior {
	pub(crate) fn can_be_bound(self) -> bool {
		matches!(self, Self::Method { .. } | Self::Function { .. })
	}

	pub(crate) fn get_name(self) -> TypeId {
		match self {
			Self::ArrowFunction { .. } => TypeId::EMPTY_STRING,
			Self::Method { name, .. }
			| Self::Function { name, .. }
			| Self::Constructor { name, .. } => name,
		}
	}
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
	/// This is the item type, aka the `T` of `Array<T>`
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

use std::{
	borrow::Cow,
	collections::{hash_map::Entry, HashMap},
	mem,
};

use source_map::{SourceId, SpanWithSource};

use crate::{
	context::{
		environment::FunctionScope, facts::Facts, get_value_of_variable, CanReferenceThis,
		ContextType, Syntax,
	},
	events::RootReference,
	types::{
		self,
		classes::ClassValue,
		functions::SynthesisedParameters,
		poly_types::GenericTypeParameters,
		properties::{PropertyKey, PropertyValue},
		Constructor, FunctionType, PolyNature, TypeStore,
	},
	CheckingData, Environment, FunctionId, ReadFromFS, Scope, Type, TypeId, VariableId,
};

#[derive(Clone, Copy, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub enum ThisValue {
	Passed(TypeId),
	#[default]
	UseParent,
}

impl ThisValue {
	pub(crate) fn get(
		self,
		environment: &mut Environment,
		types: &TypeStore,
		position: &SpanWithSource,
	) -> TypeId {
		match self {
			ThisValue::Passed(value) => value,
			ThisValue::UseParent => environment.get_value_of_this(types, position),
		}
	}

	pub(crate) fn get_passed(self) -> Option<TypeId> {
		match self {
			ThisValue::Passed(value) => Some(value),
			ThisValue::UseParent => None,
		}
	}
}

pub enum GetterSetter {
	Getter,
	Setter,
	None,
}

pub fn register_arrow_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	expecting: TypeId,
	is_async: bool,
	function: &impl SynthesisableFunction<A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> TypeId {
	let function_type = environment.new_function(
		checking_data,
		function,
		FunctionRegisterBehavior::ArrowFunction { expecting, is_async },
	);
	checking_data.types.new_function_type(function_type)
}

pub fn register_expression_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	expecting: TypeId,
	is_async: bool,
	is_generator: bool,
	location: Option<String>,
	function: &impl SynthesisableFunction<A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> TypeId {
	let function_type = environment.new_function(
		checking_data,
		function,
		FunctionRegisterBehavior::ExpressionFunction {
			expecting,
			is_async,
			is_generator,
			location,
		},
	);
	checking_data.types.new_function_type(function_type)
}

pub fn synthesise_hoisted_statement_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	variable_id: crate::VariableId,
	is_async: bool,
	is_generator: bool,
	location: Option<String>,
	function: &impl SynthesisableFunction<A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	// TODO get existing by variable_id
	let behavior = crate::behavior::functions::FunctionRegisterBehavior::StatementFunction {
		hoisted: variable_id,
		is_async,
		is_generator,
		location,
	};

	let function = environment.new_function(checking_data, function, behavior);
	environment
		.facts
		.variable_current_value
		.insert(variable_id, checking_data.types.new_function_type(function));
}

pub fn function_to_property(
	getter_setter: &GetterSetter,
	function: FunctionType,
	types: &mut TypeStore,
) -> PropertyValue {
	match getter_setter {
		GetterSetter::Getter => PropertyValue::Getter(Box::new(function)),
		GetterSetter::Setter => PropertyValue::Setter(Box::new(function)),
		GetterSetter::None => PropertyValue::Value(types.new_function_type(function)),
	}
}

/// TODO different place
/// TODO maybe generic
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionBehavior {
	/// For arrow functions, cannot have `this` bound
	ArrowFunction {
		is_async: bool,
	},
	Method {
		free_this_id: TypeId,
		is_async: bool,
		is_generator: bool,
	},
	// Functions defined `function`. Extends above by allowing `new`
	Function {
		/// IMPORTANT THIS DOES NOT POINT TO THE OBJECT
		free_this_id: TypeId,
		is_async: bool,
		is_generator: bool,
	},
	// Constructors, always new
	Constructor {
		/// None is super exists, as that is handled by events
		non_super_prototype: Option<TypeId>,
		/// The id of the generic that needs to be pulled out
		this_object_type: TypeId,
	},
}

impl FunctionBehavior {
	pub(crate) fn can_be_bound(&self) -> bool {
		matches!(self, Self::Method { .. } | Self::Function { .. })
	}
}

/// Covers both actual functions and
pub trait SynthesisableFunction<A: crate::ASTImplementation> {
	fn id(&self, source_id: SourceId) -> FunctionId;

	// TODO temp
	fn has_body(&self) -> bool;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn type_parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<GenericTypeParameters>;

	fn this_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<TypeId>;

	/// For object literals
	fn super_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<TypeId>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
		expected_parameters: Option<&SynthesisedParameters>,
	) -> SynthesisedParameters;

	fn return_type_annotation<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<(TypeId, SpanWithSource)>;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	);
}

/// TODO might be generic if [`FunctionBehavior`] becomes generic
pub enum FunctionRegisterBehavior<'a, A: crate::ASTImplementation> {
	ArrowFunction {
		expecting: TypeId,
		is_async: bool,
	},
	ExpressionFunction {
		expecting: TypeId,
		is_async: bool,
		is_generator: bool,
		location: Option<String>,
	},
	StatementFunction {
		hoisted: VariableId,
		is_async: bool,
		is_generator: bool,
		location: Option<String>,
	},
	// TODO this will take PartialFunction
	ObjectMethod {
		is_async: bool,
		is_generator: bool,
		// location: Option<String>,
	},
	// TODO this will take PartialFunction
	ClassMethod {
		is_async: bool,
		is_generator: bool,
		super_type: Option<TypeId>,
		// location: Option<String>,
	},
	Constructor {
		prototype: TypeId,
		/// Is this is_some then can use `super()`
		super_type: Option<TypeId>,
		properties: ClassPropertiesToRegister<'a, A>,
	},
}

pub struct ClassPropertiesToRegister<'a, A: crate::ASTImplementation>(pub Vec<ClassValue<'a, A>>);

impl<'a, A: crate::ASTImplementation> FunctionRegisterBehavior<'a, A> {
	#[must_use]
	pub fn is_async(&self) -> bool {
		match self {
			FunctionRegisterBehavior::ArrowFunction { is_async, .. }
			| FunctionRegisterBehavior::ExpressionFunction { is_async, .. }
			| FunctionRegisterBehavior::StatementFunction { is_async, .. }
			| FunctionRegisterBehavior::ObjectMethod { is_async, .. }
			| FunctionRegisterBehavior::ClassMethod { is_async, .. } => *is_async,
			FunctionRegisterBehavior::Constructor { .. } => false,
		}
	}

	#[must_use]
	pub fn is_generator(&self) -> bool {
		match self {
			FunctionRegisterBehavior::ExpressionFunction { is_generator, .. }
			| FunctionRegisterBehavior::StatementFunction { is_generator, .. }
			| FunctionRegisterBehavior::ObjectMethod { is_generator, .. }
			| FunctionRegisterBehavior::ClassMethod { is_generator, .. } => *is_generator,
			FunctionRegisterBehavior::ArrowFunction { .. }
			| FunctionRegisterBehavior::Constructor { .. } => false,
		}
	}
}

#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct ClosedOverVariables(pub(crate) HashMap<VariableId, TypeId>);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub struct ClosureId(pub(crate) u32);

pub trait ClosureChain {
	fn get_fact_from_closure<T, R>(&self, fact: &Facts, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>;
}

pub(crate) fn register_function<T, A, F>(
	context: &mut Environment,
	behavior: FunctionRegisterBehavior<A>,
	function: &F,
	checking_data: &mut CheckingData<T, A>,
) -> FunctionType
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
	F: SynthesisableFunction<A>,
{
	let _is_async = behavior.is_async();
	let _is_generator = behavior.is_generator();

	let (mut behavior, scope, constructor, location, expected_parameters, _expected_return) =
		match behavior {
			FunctionRegisterBehavior::Constructor { super_type, prototype, properties } => (
				FunctionBehavior::Constructor {
					non_super_prototype: super_type.is_some().then_some(prototype),
					this_object_type: TypeId::ERROR_TYPE,
				},
				FunctionScope::Constructor {
					extends: super_type.is_some(),
					type_of_super: super_type,
					this_object_type: TypeId::ERROR_TYPE,
				},
				Some((prototype, properties)),
				None,
				None,
				None,
			),
			FunctionRegisterBehavior::ArrowFunction { expecting, is_async } => {
				crate::utils::notify!("expecting {:?}", expecting);
				let (expecting_parameters, expected_return) =
					if let Type::FunctionReference(func_id, _) =
						checking_data.types.get_type_by_id(expecting)
					{
						let f = checking_data.types.get_function_from_id(*func_id);
						(Some(f.parameters.clone()), Some(f.return_type))
					} else {
						(None, None)
					};

				(
					FunctionBehavior::ArrowFunction { is_async },
					// to set
					FunctionScope::ArrowFunction { free_this_type: TypeId::ERROR_TYPE, is_async },
					None,
					None,
					expecting_parameters,
					expected_return,
				)
			}
			FunctionRegisterBehavior::ExpressionFunction {
				expecting: _,
				is_async,
				is_generator,
				location,
			} => (
				FunctionBehavior::Function {
					is_async,
					is_generator,
					free_this_id: TypeId::ERROR_TYPE,
				},
				FunctionScope::Function {
					is_generator,
					is_async,
					// to set
					this_type: TypeId::ERROR_TYPE,
					type_of_super: TypeId::ANY_TYPE,
				},
				None,
				location,
				None,
				None,
			),
			FunctionRegisterBehavior::StatementFunction {
				hoisted: _,
				is_async,
				is_generator,
				location,
			} => (
				FunctionBehavior::Function {
					is_async,
					is_generator,
					free_this_id: TypeId::ERROR_TYPE,
				},
				FunctionScope::Function {
					is_generator,
					is_async,
					this_type: TypeId::ERROR_TYPE,
					type_of_super: TypeId::ERROR_TYPE,
				},
				None,
				location,
				None,
				None,
			),
			FunctionRegisterBehavior::ClassMethod { is_async, is_generator, super_type: _ }
			| FunctionRegisterBehavior::ObjectMethod { is_async, is_generator } => (
				FunctionBehavior::Method {
					is_async,
					is_generator,
					free_this_id: TypeId::ERROR_TYPE,
				},
				// TODO eager super
				FunctionScope::MethodFunction {
					free_this_type: TypeId::ERROR_TYPE,
					is_async,
					is_generator,
				},
				None,
				None,
				None,
				None,
			),
		};

	let mut function_environment = context.new_lexical_environment(Scope::Function(scope));

	let type_parameters = function.type_parameters(&mut function_environment, checking_data);

	// TODO should be in function, but then requires mutable environment :(
	let this_constraint = function.this_constraint(&mut function_environment, checking_data);

	// `this` changes stuff
	if let Scope::Function(ref mut scope) = function_environment.context_type.scope {
		match scope {
			FunctionScope::ArrowFunction { ref mut free_this_type, .. }
			| FunctionScope::MethodFunction { ref mut free_this_type, .. } => {
				let type_id = if let Some(tc) = this_constraint {
					checking_data.types.register_type(Type::RootPolyType(
						PolyNature::FreeVariable { reference: RootReference::This, based_on: tc },
					))
				} else {
					TypeId::ANY_INFERRED_FREE_THIS
				};
				if let FunctionBehavior::Method { ref mut free_this_id, .. } = behavior {
					*free_this_id = type_id;
				}
				*free_this_type = type_id;
			}
			FunctionScope::Function { ref mut this_type, .. } => {
				// TODO this could be done conditionally to create less objects, but also doesn't introduce any bad side effects so
				// TODO prototype needs to be a poly based on this.prototype. This also fixes inference

				let (this_free_variable, this_constructed_object) =
					if let Some(this_constraint) = this_constraint {
						// TODO I don't whether NEW_TARGET_ARG should have a backer
						let prototype = checking_data.types.register_type(Type::Constructor(
							Constructor::Property {
								on: TypeId::NEW_TARGET_ARG,
								under: PropertyKey::String(Cow::Owned("value".to_owned())),
								result: this_constraint,
							},
						));

						let this_constructed_object = function_environment.facts.new_object(
							Some(prototype),
							&mut checking_data.types,
							true,
							true,
						);

						let this_free_variable = checking_data.types.register_type(
							Type::RootPolyType(PolyNature::FreeVariable {
								reference: RootReference::This,
								based_on: this_constraint,
							}),
						);

						(this_free_variable, this_constructed_object)
					} else {
						// TODO inferred prototype
						let this_constructed_object = function_environment.facts.new_object(
							None,
							&mut checking_data.types,
							true,
							true,
						);
						(TypeId::ANY_INFERRED_FREE_THIS, this_constructed_object)
					};

				if let FunctionBehavior::Function { ref mut free_this_id, .. } = behavior {
					// TODO set object as well
					*free_this_id = this_free_variable;
				}

				let new_conditional_type = checking_data.types.new_conditional_type(
					TypeId::NEW_TARGET_ARG,
					this_constructed_object,
					this_free_variable,
				);

				// TODO set super type as well

				// TODO what is the union, shouldn't it be the this_constraint?
				*this_type = new_conditional_type;
			}
			FunctionScope::Constructor {
				extends: _,
				type_of_super: _,
				ref mut this_object_type,
			} => {
				crate::utils::notify!("Setting 'this' type here");
				if let Some((prototype, properties)) = constructor {
					let new_this_object_type = types::create_this_before_function_synthesis(
						&mut checking_data.types,
						&mut function_environment.facts,
						prototype,
					);

					*this_object_type = new_this_object_type;
					// TODO super/derived behavior
					types::classes::register_properties_into_environment(
						&mut function_environment,
						new_this_object_type,
						checking_data,
						properties,
					);
					function_environment.can_reference_this = CanReferenceThis::ConstructorCalled;

					if let FunctionBehavior::Constructor { ref mut this_object_type, .. } = behavior
					{
						crate::utils::notify!("Set this object type");
						*this_object_type = new_this_object_type;
					} else {
						unreachable!()
					}
				} else {
					unreachable!()
				}
			}
		}
	} else {
		unreachable!()
	}

	// TODO reuse existing if hoisted or can be sent down
	let synthesised_parameters =
		function.parameters(&mut function_environment, checking_data, expected_parameters.as_ref());

	let return_type_annotation =
		function.return_type_annotation(&mut function_environment, checking_data);

	// let _expected_return_type: Option<TypeId> = expected_return;
	function_environment.context_type.location = location;

	let returned = if function.has_body() {
		function.body(&mut function_environment, checking_data);
		// Temporary move events to satisfy borrow checker
		let events = mem::take(&mut function_environment.facts.events);

		let returned = crate::events::helpers::get_return_from_events(
			&mut events.iter(),
			checking_data,
			// TODO environment should be good enough, but needs environment not context
			&mut function_environment,
			return_type_annotation,
		);
		function_environment.facts.events = events;

		match returned {
			crate::events::helpers::ReturnedTypeFromBlock::ContinuedExecution => {
				TypeId::UNDEFINED_TYPE
			}
			crate::events::helpers::ReturnedTypeFromBlock::ReturnedIf { when, returns } => {
				checking_data.types.new_conditional_type(when, returns, TypeId::UNDEFINED_TYPE)
			}
			crate::events::helpers::ReturnedTypeFromBlock::Returned(ty) => ty,
		}
	} else {
		return_type_annotation.map_or(TypeId::UNDEFINED_TYPE, |(left, _)| left)
	};

	let closes_over = function_environment
		.context_type
		.closed_over_references
		.iter()
		.map(|reference| {
			let ty = match reference {
				RootReference::Variable(on) => {
					let get_value_of_variable = get_value_of_variable(
						function_environment.facts_chain(),
						*on,
						None::<&crate::types::poly_types::FunctionTypeArguments>,
					);
					get_value_of_variable.expect("value not assigned?")
				}
				// TODO not sure
				RootReference::This => TypeId::ANY_INFERRED_FREE_THIS,
			};

			(reference.clone(), ty)
		})
		.collect();

	let Syntax { free_variables, closed_over_references: function_closes_over, .. } =
		function_environment.context_type;

	let facts = function_environment.facts;

	context.variable_names.extend(function_environment.variable_names);

	// TODO temp ...
	for (on, properties) in facts.current_properties {
		match context.facts.current_properties.entry(on) {
			Entry::Occupied(_occupied) => {}
			Entry::Vacant(vacant) => {
				vacant.insert(properties);
			}
		}
	}

	for (on, properties) in facts.closure_current_values {
		match context.facts.closure_current_values.entry(on) {
			Entry::Occupied(_occupied) => {}
			Entry::Vacant(vacant) => {
				vacant.insert(properties);
			}
		}
	}

	if let Some(closed_over_variables) = context.context_type.get_closed_over_references() {
		closed_over_variables.extend(free_variables.iter().cloned());
		// TODO not sure, but fixes nesting
		closed_over_variables.extend(function_closes_over.iter().cloned());
	}

	// TODO should references used in the function be counted in this scope
	// might break the checking though

	let free_variables = free_variables
		.into_iter()
		.map(|reference| {
			// TODO get the restriction from the context type
			(reference, TypeId::ANY_TYPE)
		})
		.collect();

	let id = function.id(context.get_source());

	FunctionType {
		id,
		constant_function: None,
		behavior,
		type_parameters,
		parameters: synthesised_parameters,
		return_type: returned,
		effects: facts.events,
		free_variables,
		closed_over_variables: closes_over,
	}
}

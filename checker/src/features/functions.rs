use std::{
	borrow::Cow,
	collections::{hash_map::Entry, HashMap},
};

use source_map::{SourceId, SpanWithSource};

use crate::{
	context::{
		environment::{ContextLocation, ExpectedReturnType, FunctionScope},
		get_on_ctx, get_value_of_variable,
		information::{merge_info, LocalInformation},
		CanReferenceThis, ContextType, Syntax,
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	events::RootReference,
	subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
	types::{
		self,
		classes::ClassValue,
		functions::SynthesisedParameters,
		poly_types::GenericTypeParameters,
		printing::print_type,
		properties::{PropertyKey, PropertyValue},
		substitute, Constructor, FunctionEffect, FunctionType, InternalFunctionEffect, PolyNature,
		StructureGenerics, SynthesisedParameter, SynthesisedRestParameter, TypeStore,
	},
	ASTImplementation, CheckingData, Environment, FunctionId, GeneralContext, ReadFromFS, Scope,
	Type, TypeId, VariableId,
};

#[derive(Clone, Copy, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub enum ThisValue {
	Passed(TypeId),
	/// Or pick from [`Constructor::Property`]
	#[default]
	UseParent,
}

impl ThisValue {
	pub(crate) fn get(
		self,
		environment: &mut Environment,
		types: &TypeStore,
		position: SpanWithSource,
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

#[derive(Debug, Clone, Copy)]
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
	let function_type = synthesise_function(
		function,
		FunctionRegisterBehavior::ArrowFunction { expecting, is_async },
		environment,
		checking_data,
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
	let function_type = synthesise_function(
		function,
		FunctionRegisterBehavior::ExpressionFunction {
			expecting,
			is_async,
			is_generator,
			location,
		},
		environment,
		checking_data,
	);
	checking_data.types.new_function_type(function_type)
}

pub fn synthesise_hoisted_statement_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	variable_id: crate::VariableId,
	is_async: bool,
	is_generator: bool,
	location: ContextLocation,
	function: &impl SynthesisableFunction<A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	if !function.has_body() {
		checking_data.raise_unimplemented_error(
			"Overloaded function",
			function.get_position().with_source(environment.get_source()),
		);
		return;
	}

	let behavior = FunctionRegisterBehavior::StatementFunction {
		hoisted: variable_id,
		is_async,
		is_generator,
		location,
		internal_marker: None,
	};

	let function = synthesise_function(function, behavior, environment, checking_data);
	environment
		.info
		.variable_current_value
		.insert(variable_id, checking_data.types.new_function_type(function));
}

#[allow(clippy::too_many_arguments)]
pub fn synthesise_declare_statement_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	variable_id: crate::VariableId,
	is_async: bool,
	is_generator: bool,
	location: Option<String>,
	internal_marker: Option<InternalFunctionEffect>,
	function: &impl SynthesisableFunction<A>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) {
	let behavior = FunctionRegisterBehavior::StatementFunction {
		hoisted: variable_id,
		is_async,
		is_generator,
		location,
		internal_marker,
	};

	let function = synthesise_function(function, behavior, environment, checking_data);
	environment
		.info
		.variable_current_value
		.insert(variable_id, checking_data.types.new_function_type(function));
}

pub fn function_to_property(
	getter_setter: GetterSetter,
	function: FunctionType,
	types: &mut TypeStore,
	is_declare: bool,
) -> PropertyValue {
	match getter_setter {
		GetterSetter::Getter => PropertyValue::Getter(Box::new(function)),
		GetterSetter::Setter => PropertyValue::Setter(Box::new(function)),
		GetterSetter::None => PropertyValue::Value(
			if is_declare && matches!(function.effect, FunctionEffect::Unknown) {
				types.new_hoisted_function_type(function)
			} else {
				types.new_function_type(function)
			},
		),
	}
}

pub fn synthesise_function_default_value<'a, T: crate::ReadFromFS, A: ASTImplementation>(
	parameter_ty: TypeId,
	parameter_constraint: TypeId,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	expression: &'a A::Expression<'a>,
) -> TypeId {
	let (value, out, ..) = environment.new_lexical_environment_fold_into_parent(
		Scope::DefaultFunctionParameter {},
		checking_data,
		|environment, checking_data| {
			A::synthesise_expression(expression, parameter_constraint, environment, checking_data)
		},
	);

	let mut basic_equality = BasicEquality::default();

	let result = type_is_subtype(
		parameter_constraint,
		value,
		&mut basic_equality,
		environment,
		&checking_data.types,
	);

	if let SubTypeResult::IsNotSubType(_) = result {
		let expected = TypeStringRepresentation::from_type_id(
			parameter_ty,
			environment,
			&checking_data.types,
			false,
		);

		let found =
			TypeStringRepresentation::from_type_id(value, environment, &checking_data.types, false);
		let at = A::expression_position(expression).with_source(environment.get_source());

		checking_data.diagnostics_container.add_error(TypeCheckError::InvalidDefaultParameter {
			at,
			expected,
			found,
		});
	}

	// Abstraction of `typeof parameter === "undefined"` to generate less types.
	let is_undefined_condition = checking_data.types.register_type(Type::Constructor(
		Constructor::TypeRelationOperator(types::TypeRelationOperator::Extends {
			ty: parameter_ty,
			extends: TypeId::UNDEFINED_TYPE,
		}),
	));

	// TODO is this needed
	// let union = checking_data.types.new_or_type(parameter_ty, value);

	let result =
		checking_data.types.register_type(Type::Constructor(Constructor::ConditionalResult {
			condition: is_undefined_condition,
			truthy_result: value,
			otherwise_result: parameter_ty,
			result_union: parameter_constraint,
		}));

	// TODO don't share parent
	let Some(GeneralContext::Syntax(parent)) = environment.context_type.get_parent() else {
		unreachable!()
	};
	merge_info(
		*parent,
		&mut environment.info,
		is_undefined_condition,
		out.unwrap().0,
		None,
		&mut checking_data.types,
	);

	result
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
	},
	/// Functions defined `function`. Extends above by allowing `new`
	Function {
		/// IMPORTANT THIS DOES NOT POINT TO THE OBJECT
		free_this_id: TypeId,
		is_async: bool,
		is_generator: bool,
	},
	/// Constructors, always new
	Constructor {
		/// None is super exists, as that is handled by events
		non_super_prototype: Option<TypeId>,
		/// The id of the generic that needs to be pulled out
		this_object_type: TypeId,
	},
}

impl FunctionBehavior {
	pub(crate) fn can_be_bound(self) -> bool {
		matches!(self, Self::Method { .. } | Self::Function { .. })
	}
}

#[derive(Clone, Copy)]
pub struct ReturnType(pub TypeId, pub SpanWithSource);

pub struct PartialFunction(
	pub Option<GenericTypeParameters>,
	pub SynthesisedParameters,
	pub Option<ReturnType>,
);

/// Covers both actual functions and
pub trait SynthesisableFunction<A: crate::ASTImplementation> {
	fn id(&self, source_id: SourceId) -> FunctionId {
		FunctionId(source_id, self.get_position().start)
	}

	/// For debugging only
	fn get_name(&self) -> Option<&str>;

	/// For debugging only
	fn get_position(&self) -> source_map::Span;

	// TODO temp
	fn has_body(&self) -> bool;

	// /// For detecting what is inside
	// fn get_body_span(&self) -> source_map::Span;

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
	) -> Option<ReturnType>;

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
		location: ContextLocation,
	},
	StatementFunction {
		hoisted: VariableId,
		is_async: bool,
		is_generator: bool,
		location: ContextLocation,
		internal_marker: Option<InternalFunctionEffect>,
	},
	ObjectMethod {
		// TODO this will take PartialFunction from hoisted?
		expecting: TypeId,
		is_async: bool,
		is_generator: bool,
		// location: ContextLocation,
	},
	ClassMethod {
		// TODO this will take PartialFunction from hoisted?
		expecting: TypeId,
		is_async: bool,
		is_generator: bool,
		super_type: Option<TypeId>,
		internal_marker: Option<InternalFunctionEffect>,
		/// Used for shape of `this`
		this_shape: TypeId,
	},
	Constructor {
		prototype: TypeId,
		/// Is this [`Option::is_some`] then can use `super()`
		super_type: Option<TypeId>,
		properties: ClassPropertiesToRegister<'a, A>,
	},
}

pub struct ClassPropertiesToRegister<'a, A: crate::ASTImplementation> {
	pub properties: Vec<ClassValue<'a, A>>,
}

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
	fn get_fact_from_closure<T, R>(&self, fact: &LocalInformation, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>;
}

pub(crate) fn synthesise_function<T, A, F>(
	function: &F,
	behavior: FunctionRegisterBehavior<A>,
	base_environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
) -> FunctionType
where
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
	F: SynthesisableFunction<A>,
{
	struct FunctionKind<'a, A: crate::ASTImplementation> {
		pub(super) behavior: FunctionBehavior,
		pub(super) scope: FunctionScope,
		pub(super) internal: Option<InternalFunctionEffect>,
		/// TODO wip
		pub(super) constructor: Option<(TypeId, ClassPropertiesToRegister<'a, A>)>,
		pub(super) expected_parameters: Option<SynthesisedParameters>,
		pub(super) this_shape: Option<TypeId>,
	}

	let _is_async = behavior.is_async();
	let _is_generator = behavior.is_generator();

	// unfold information from the behavior
	let kind: FunctionKind<A> = match behavior {
		FunctionRegisterBehavior::Constructor { super_type, prototype, properties } => {
			FunctionKind {
				behavior: FunctionBehavior::Constructor {
					non_super_prototype: super_type.is_some().then_some(prototype),
					this_object_type: TypeId::ERROR_TYPE,
				},
				scope: FunctionScope::Constructor {
					extends: super_type.is_some(),
					type_of_super: super_type,
					this_object_type: TypeId::ERROR_TYPE,
				},
				internal: None,
				constructor: Some((prototype, properties)),
				expected_parameters: None,
				// TODO
				this_shape: None,
			}
		}
		FunctionRegisterBehavior::ArrowFunction { expecting, is_async } => {
			// crate::utils::notify!(
			// 	"expecting {}",
			// 	types::printing::print_type(
			// 		expecting,
			// 		&checking_data.types,
			// 		base_environment,
			// 		false
			// 	)
			// );
			let (expected_parameters, expected_return) = get_expected_parameters_from_type(
				expecting,
				&mut checking_data.types,
				base_environment,
			);

			if let Some((or, _)) =
				expected_parameters.as_ref().and_then(|a| a.get_parameter_type_at_index(0))
			{
				crate::utils::notify!(
					"First expected parameter {:?}",
					print_type(or, &checking_data.types, base_environment, true)
				);
			}

			FunctionKind {
				behavior: FunctionBehavior::ArrowFunction { is_async },
				scope: FunctionScope::ArrowFunction {
					free_this_type: TypeId::ERROR_TYPE,
					is_async,
					expected_return: expected_return.map(ExpectedReturnType::Inferred),
				},
				internal: None,
				constructor: None,
				expected_parameters,
				this_shape: None,
			}
		}
		FunctionRegisterBehavior::ExpressionFunction {
			expecting,
			is_async,
			is_generator,
			location,
		} => {
			let (expected_parameters, expected_return) = get_expected_parameters_from_type(
				expecting,
				&mut checking_data.types,
				base_environment,
			);
			FunctionKind {
				behavior: FunctionBehavior::Function {
					is_async,
					is_generator,
					free_this_id: TypeId::ERROR_TYPE,
				},
				scope: FunctionScope::Function {
					is_generator,
					is_async,
					// to set
					this_type: TypeId::ERROR_TYPE,
					type_of_super: TypeId::ANY_TYPE,
					expected_return: expected_return.map(ExpectedReturnType::Inferred),
					location,
				},
				internal: None,
				constructor: None,
				expected_parameters,
				this_shape: None,
			}
		}
		FunctionRegisterBehavior::StatementFunction {
			hoisted: _,
			is_async,
			is_generator,
			location,
			internal_marker,
		} => FunctionKind {
			behavior: FunctionBehavior::Function {
				is_async,
				is_generator,
				free_this_id: TypeId::ERROR_TYPE,
			},
			scope: FunctionScope::Function {
				is_generator,
				is_async,
				this_type: TypeId::ERROR_TYPE,
				type_of_super: TypeId::ERROR_TYPE,
				expected_return: None,
				location,
			},
			internal: internal_marker,
			constructor: None,
			expected_parameters: None,
			this_shape: None,
		},
		FunctionRegisterBehavior::ClassMethod {
			is_async,
			is_generator,
			super_type: _,
			expecting,
			internal_marker,
			this_shape,
		} => {
			let (expected_parameters, expected_return) = get_expected_parameters_from_type(
				expecting,
				&mut checking_data.types,
				base_environment,
			);

			FunctionKind {
				behavior: FunctionBehavior::Method {
					is_async,
					is_generator,
					free_this_id: this_shape,
				},
				scope: FunctionScope::MethodFunction {
					free_this_type: this_shape,
					is_async,
					is_generator,
					expected_return: expected_return.map(ExpectedReturnType::Inferred),
				},
				internal: internal_marker,
				constructor: None,
				expected_parameters,
				this_shape: Some(this_shape),
			}
		}
		FunctionRegisterBehavior::ObjectMethod { is_async, is_generator, expecting } => {
			let (expected_parameters, expected_return) = get_expected_parameters_from_type(
				expecting,
				&mut checking_data.types,
				base_environment,
			);

			FunctionKind {
				behavior: FunctionBehavior::Method {
					is_async,
					is_generator,
					free_this_id: TypeId::ERROR_TYPE,
				},
				scope: FunctionScope::MethodFunction {
					free_this_type: TypeId::ERROR_TYPE,
					is_async,
					is_generator,
					expected_return: expected_return.map(ExpectedReturnType::Inferred),
				},
				internal: None,
				constructor: None,
				expected_parameters,
				// TODO could be something in the future
				this_shape: None,
			}
		}
	};

	let id = function.id(base_environment.get_source());
	let FunctionKind {
		mut behavior,
		scope,
		internal,
		constructor,
		expected_parameters,
		this_shape,
	} = kind;

	let mut function_environment = base_environment.new_lexical_environment(Scope::Function(scope));

	if function.has_body() {
		let type_parameters = function.type_parameters(&mut function_environment, checking_data);

		// TODO should be in function, but then requires mutable environment :(
		let this_constraint =
			function.this_constraint(&mut function_environment, checking_data).or(this_shape);

		// `this` changes stuff
		if let Scope::Function(ref mut scope) = function_environment.context_type.scope {
			match scope {
				FunctionScope::ArrowFunction { ref mut free_this_type, .. }
				| FunctionScope::MethodFunction { ref mut free_this_type, .. } => {
					let type_id = if let Some(tc) = this_constraint {
						checking_data.types.register_type(Type::RootPolyType(
							PolyNature::FreeVariable {
								reference: RootReference::This,
								based_on: tc,
							},
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
					// TODO temp to reduce types

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
									bind_this: true,
								},
							));

							let this_constructed_object = function_environment.info.new_object(
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
							let this_constructed_object = function_environment.info.new_object(
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
							&mut function_environment.info,
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

						function_environment.can_reference_this =
							CanReferenceThis::ConstructorCalled;

						if let FunctionBehavior::Constructor { ref mut this_object_type, .. } =
							behavior
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
		let synthesised_parameters = function.parameters(
			&mut function_environment,
			checking_data,
			expected_parameters.as_ref(),
		);

		let return_type_annotation =
			function.return_type_annotation(&mut function_environment, checking_data);

		{
			// Add expected return type
			if let Scope::Function(ref mut scope) = function_environment.context_type.scope {
				if !matches!(scope, FunctionScope::Constructor { .. }) {
					if let (expect @ None, Some(ReturnType(return_type_annotation, pos))) =
						(scope.get_expected_return_type_mut(), return_type_annotation)
					{
						*expect = Some(ExpectedReturnType::FromReturnAnnotation(
							return_type_annotation,
							// TODO lol
							pos.without_source(),
						));
					}
				}
			}
		}

		function.body(&mut function_environment, checking_data);

		let iter = function_environment.context_type.closed_over_references.iter();

		let closes_over: HashMap<_, _> = iter
			.map(|reference| {
				match reference {
					RootReference::Variable(on) => {
						let get_value_of_variable = get_value_of_variable(
							&function_environment,
							*on,
							None::<&crate::types::poly_types::FunctionTypeArguments>,
						);
						let ty = if let Some(value) = get_value_of_variable {
							value
						} else {
							// TODO think we are getting rid of this
							// let name = function_environment.get_variable_name(*on);
							// checking_data.diagnostics_container.add_error(
							// 	TypeCheckError::UnreachableVariableClosedOver(
							// 		name.to_string(),
							// 		function
							// 			.get_position()
							// 			.with_source(base_environment.get_source()),
							// 	),
							// );

							// `TypeId::ERROR_TYPE` is also okay
							TypeId::NEVER_TYPE
						};
						(*on, ty)
					}
					// TODO unsure
					RootReference::This => todo!(),
				}
			})
			.collect();

		let closes_over = ClosedOverVariables(closes_over);

		let Syntax { free_variables, closed_over_references: function_closes_over, state, .. } =
			function_environment.context_type;

		let returned = if function.has_body() {
			state.returned_type(&mut checking_data.types)
		} else {
			return_type_annotation.map_or(TypeId::UNDEFINED_TYPE, |ReturnType(ty, _)| ty)
		};

		// crate::utils::notify!(
		// 	"closes_over {:?}, free_variable {:?}, in {:?}",
		// 	closes_over,
		// 	free_variables,
		// 	function.get_name()
		// );

		let info = function_environment.info;
		let variable_names = function_environment.variable_names;

		// TODO this fixes properties being lost during printing and subtyping
		for (on, properties) in info.current_properties {
			match base_environment.info.current_properties.entry(on) {
				Entry::Occupied(_occupied) => {}
				Entry::Vacant(vacant) => {
					vacant.insert(properties);
				}
			}
		}

		for (on, properties) in info.closure_current_values {
			match base_environment.info.closure_current_values.entry(on) {
				Entry::Occupied(_occupied) => {}
				Entry::Vacant(vacant) => {
					vacant.insert(properties);
				}
			}
		}

		// TODO collect here because of lifetime mutation issues from closed over
		let continues_to_close_over = function_closes_over
			.into_iter()
			.filter(|r| match r {
				RootReference::Variable(id) => {
					// Keep if body does not contain id
					let contains = base_environment
						.parents_iter()
						.any(|c| get_on_ctx!(&c.variable_names).contains_key(id));

					crate::utils::notify!("v-id {:?} con {:?}", id, contains);
					contains
				}
				RootReference::This => !behavior.can_be_bound(),
			})
			.collect::<Vec<_>>();

		if let Some(closed_over_variables) =
			base_environment.context_type.get_closed_over_references_mut()
		{
			closed_over_variables.extend(free_variables.iter().cloned());
			closed_over_variables.extend(continues_to_close_over);
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

		// TODO why
		base_environment.variable_names.extend(variable_names);

		// While could just use returned, if it uses the annotation as the return type
		let return_type = return_type_annotation.map_or(returned, |ReturnType(ty, _)| ty);

		let effect = FunctionEffect::SideEffects {
			events: info.events,
			free_variables,
			closed_over_variables: closes_over,
		};

		FunctionType {
			id,
			behavior,
			type_parameters,
			parameters: synthesised_parameters,
			return_type,
			effect,
		}
	} else {
		// TODO this might not need to create a new environment if no type parameters AND the parameters don't get added to environment
		let type_parameters = function.type_parameters(&mut function_environment, checking_data);

		// TODO DO NOT ASSIGN
		let parameters = function.parameters(
			&mut function_environment,
			checking_data,
			expected_parameters.as_ref(),
		);

		let return_type = function
			.return_type_annotation(&mut function_environment, checking_data)
			.map_or(TypeId::ANY_TYPE, |ReturnType(ty, _)| ty);

		for (on, properties) in function_environment.info.current_properties {
			match base_environment.info.current_properties.entry(on) {
				Entry::Occupied(_occupied) => {}
				Entry::Vacant(vacant) => {
					vacant.insert(properties);
				}
			}
		}

		let effect = match internal {
			Some(InternalFunctionEffect::Constant(identifier)) => {
				FunctionEffect::Constant(identifier)
			}
			Some(InternalFunctionEffect::InputOutput(identifier)) => {
				FunctionEffect::InputOutput(identifier)
			}
			None => FunctionEffect::Unknown,
		};

		FunctionType { id, type_parameters, parameters, return_type, behavior, effect }
	}
}

fn get_expected_parameters_from_type(
	expecting: TypeId,
	types: &mut TypeStore,
	environment: &mut Environment,
) -> (Option<SynthesisedParameters>, Option<TypeId>) {
	let ty = types.get_type_by_id(expecting);
	if let Type::FunctionReference(func_id) = ty {
		let f = types.get_function_from_id(*func_id);
		(Some(f.parameters.clone()), Some(f.return_type))
	} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
		arguments,
		on,
	})) = ty
	{
		let mut structure_generic_arguments = arguments.clone();

		let (expected_parameters, expected_return_type) =
			get_expected_parameters_from_type(*on, types, environment);

		(
			expected_parameters.map(|e| SynthesisedParameters {
				parameters: e
					.parameters
					.into_iter()
					.map(|p| SynthesisedParameter {
						ty: substitute(p.ty, &mut structure_generic_arguments, environment, types),
						..p
					})
					.collect(),
				rest_parameter: e.rest_parameter.map(|rp| SynthesisedRestParameter {
					item_type: substitute(
						rp.item_type,
						&mut structure_generic_arguments,
						environment,
						types,
					),
					..rp
				}),
			}),
			expected_return_type
				.map(|rt| substitute(rt, &mut structure_generic_arguments, environment, types)),
		)
	} else {
		(None, None)
	}
}

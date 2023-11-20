use std::{collections::HashMap, mem};

use source_map::{SourceId, Span, SpanWithSource};

use super::variables::VariableMutability;
use crate::{
	context::{
		facts::{Facts, Publicity},
		Context, ContextType,
	},
	events::Event,
	types::{
		classes::ClassValue,
		functions::SynthesisedParameters,
		poly_types::GenericTypeParameters,
		properties::{PropertyKey, PropertyValue},
		FunctionType, TypeStore,
	},
	ASTImplementation, CheckingData, Environment, FunctionId, ReadFromFS, Type, TypeId, VariableId,
};

#[derive(Clone, Copy, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub enum ThisValue {
	Passed(TypeId),
	#[default]
	UseParent,
}

impl ThisValue {
	pub(crate) fn get(
		&self,
		environment: &mut Environment,
		types: &mut TypeStore,
		position: SpanWithSource,
	) -> TypeId {
		match self {
			ThisValue::Passed(value) => *value,
			ThisValue::UseParent => environment.get_value_of_this(types, position),
		}
	}

	pub(crate) fn get_passed(&self) -> Option<TypeId> {
		match self {
			ThisValue::Passed(value) => Some(*value),
			ThisValue::UseParent => None,
		}
	}
}

pub enum GetterSetter {
	Getter,
	Setter,
	None,
}

pub fn register_arrow_function<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	expecting: TypeId,
	is_async: bool,
	function: &impl SynthesisableFunction<M>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, M>,
) -> TypeId {
	let function_type = environment.new_function(
		checking_data,
		function,
		FunctionRegisterBehavior::ArrowFunction { expecting, is_async },
	);
	checking_data.types.new_function_type(function_type)
}

pub fn register_expression_function<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	expecting: TypeId,
	is_async: bool,
	is_generator: bool,
	location: Option<String>,
	function: &impl SynthesisableFunction<M>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, M>,
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

pub fn synthesise_hoisted_statement_function<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	variable_id: crate::VariableId,
	is_async: bool,
	is_generator: bool,
	location: Option<String>,
	function: &impl SynthesisableFunction<M>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, M>,
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
	getter_setter: GetterSetter,
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
pub trait SynthesisableFunction<M: crate::ASTImplementation> {
	fn id(&self, source_id: SourceId) -> FunctionId;

	// TODO temp
	fn has_body(&self) -> bool;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn type_parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<GenericTypeParameters>;

	fn this_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<TypeId>;

	/// For object literals
	fn super_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<TypeId>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
		expected_parameters: Option<SynthesisedParameters>,
	) -> SynthesisedParameters;

	fn return_type_annotation<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<(TypeId, SpanWithSource)>;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	);
}

/// TODO might be generic if FunctionBehavior becomes generic
pub enum FunctionRegisterBehavior<'a, M: crate::ASTImplementation> {
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
		properties: ClassPropertiesToRegister<'a, M>,
	},
}

pub struct ClassPropertiesToRegister<'a, M: ASTImplementation>(pub Vec<ClassValue<'a, M>>);

impl<'a, M: crate::ASTImplementation> FunctionRegisterBehavior<'a, M> {
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

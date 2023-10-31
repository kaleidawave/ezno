use std::collections::HashMap;

use source_map::{SourceId, Span, SpanWithSource};

use super::variables::VariableMutability;
use crate::{
	context::{facts::Facts, Context, ContextType},
	events::Event,
	types::{
		functions::SynthesisedParameters, poly_types::GenericTypeParameters, properties::Property,
		FunctionType, TypeStore,
	},
	CheckingData, Environment, FunctionId, ReadFromFS, Type, TypeId, VariableId,
};

/// This is just as an API layer
pub enum MethodKind {
	Get,
	Set,
	Generator { is_async: bool },
	Async,
	Plain,
}

// pub enum FunctionKind2 {
// 	ArrowFunction { is_async: bool },
// 	StatementFunction { is_async: bool, generator: bool },
// 	ClassConstructor,
// 	Method { getter_setter_or_generator: MethodKind },
// }

/// TODO generalize for property registration
pub trait FunctionRegisterBehavior<M: crate::ASTImplementation> {
	type Return;

	/// TODO lift T
	fn function<T: SynthesisableFunction<M>, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return;
}

pub struct RegisterAsType;

impl<M: crate::ASTImplementation> FunctionRegisterBehavior<M> for RegisterAsType {
	type Return = TypeId;

	fn function<T: SynthesisableFunction<M>, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		let id = func_ty.id;
		types.functions.insert(id, func_ty);
		let ty = types.register_type(crate::Type::Function(id, ThisValue::UseParent));
		environment.facts.events.push(Event::CreateObject {
			prototype: crate::events::PrototypeArgument::Function(id),
			referenced_in_scope_as: ty,
		});
		ty
	}
}

/// Because of hoisting. On existing name
pub struct RegisterOnExisting(pub VariableId);

impl<M: crate::ASTImplementation> FunctionRegisterBehavior<M> for RegisterOnExisting {
	type Return = ();

	fn function<T: SynthesisableFunction<M>, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		context: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		let id = func_ty.id;
		types.functions.insert(id, func_ty);
		let ty = types.register_type(crate::Type::Function(id, Default::default()));
		context.facts.variable_current_value.insert(self.0, ty);
		context.facts.events.push(Event::CreateObject {
			prototype: crate::events::PrototypeArgument::Function(id),
			referenced_in_scope_as: ty,
		});
	}
}

pub struct RegisterOnExistingObject;

impl<M: crate::ASTImplementation> FunctionRegisterBehavior<M> for RegisterOnExistingObject {
	type Return = Property;

	fn function<T: SynthesisableFunction<M>, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		match func.get_kind() {
			crate::MethodKind::Get => Property::Getter(Box::new(func_ty)),
			crate::MethodKind::Set => Property::Setter(Box::new(func_ty)),
			crate::MethodKind::Async
			| crate::MethodKind::Generator { .. }
			| crate::MethodKind::Plain => {
				let id = func_ty.id;
				types.functions.insert(id, func_ty);
				let ty = types.register_type(Type::Function(id, Default::default()));
				environment.facts.events.push(Event::CreateObject {
					prototype: crate::events::PrototypeArgument::Function(id),
					referenced_in_scope_as: ty,
				});
				Property::Value(ty)
			}
		}
	}
}

pub trait SynthesisableFunction<M: crate::ASTImplementation> {
	fn is_declare(&self) -> bool;

	/// TODO vector for badness
	fn get_kind(&self) -> MethodKind;

	fn id(&self, source_id: SourceId) -> FunctionId;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn type_parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<GenericTypeParameters>;

	/// Has to be the first parameter
	fn this_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<TypeId>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> SynthesisedParameters;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	);

	fn return_type_annotation<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, M>,
	) -> Option<(TypeId, SpanWithSource)>;
}

struct ArrowFunction {
	is_async: bool,
}

struct Getter;
struct Setter;

enum Method {
	Getter,
	Setter,
	Generator { is_async: bool },
	Regular { is_async: bool },
}

struct StatementOrExpressionFunction {
	is_generator: bool,
	is_async: bool,
}

struct ClassConstructor {
	// events..?
	fields: (),
}

#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct ClosedOverVariables(pub(crate) HashMap<VariableId, TypeId>);

#[derive(Clone, Copy, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub enum ThisValue {
	Passed(TypeId),
	#[default]
	UseParent,
}

impl ThisValue {
	pub(crate) fn get(&self, environment: &mut Environment, types: &mut TypeStore) -> TypeId {
		match self {
			ThisValue::Passed(value) => *value,
			ThisValue::UseParent => environment.get_value_of_this(types),
		}
	}

	pub(crate) fn unwrap(&self) -> TypeId {
		match self {
			ThisValue::Passed(value) => *value,
			ThisValue::UseParent => panic!("Tried to get value of this"),
		}
	}
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, binary_serialize_derive::BinarySerializable)]
pub struct ClosureId(pub(crate) u32);

pub trait ClosureChain {
	fn get_fact_from_closure<T, R>(&self, fact: &Facts, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>;
}

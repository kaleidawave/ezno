use source_map::Span;

use crate::{
	context::{Context, ContextType},
	types::{
		functions::SynthesizedParameters, poly_types::GenericTypeParameters, properties::Property,
		FunctionNature, FunctionType, TypeStore,
	},
	CheckingData, Environment, FSResolver, FunctionId, Type, TypeId, VariableId,
};

#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum GetSetGeneratorOrNone {
	Get,
	Set,
	Generator,
	None,
}

pub trait RegisterBehavior {
	type Return;

	/// TODO lift T
	fn func<T: SynthesizableFunction, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return;
}

pub struct RegisterAsType;

impl RegisterBehavior for RegisterAsType {
	type Return = TypeId;

	fn func<T: SynthesizableFunction, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		types.register_type(crate::Type::Function(
			func_ty,
			crate::types::FunctionNature::Source(None),
		))
	}
}

/// Because of hoisting
pub struct RegisterOnExisting(pub String);

impl RegisterBehavior for RegisterOnExisting {
	type Return = ();

	fn func<T: SynthesizableFunction, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		let ty = types.register_type(crate::Type::Function(
			func_ty,
			crate::types::FunctionNature::Source(None),
		));
		let variable_id = environment.variables.get(&self.0).unwrap().declared_at.clone();
		environment.variable_current_value.insert(VariableId(variable_id), ty);
	}
}

pub struct RegisterOnExistingObject;

impl RegisterBehavior for RegisterOnExistingObject {
	type Return = Property;

	fn func<T: SynthesizableFunction, U: ContextType>(
		&self,
		func: &T,
		func_ty: FunctionType,
		environment: &mut Context<U>,
		types: &mut TypeStore,
	) -> Self::Return {
		match func.get_set_generator_or_none() {
			crate::GetSetGeneratorOrNone::Get => Property::Get(Box::new(func_ty)),
			crate::GetSetGeneratorOrNone::Set => Property::Set(Box::new(func_ty)),
			crate::GetSetGeneratorOrNone::Generator | crate::GetSetGeneratorOrNone::None => {
				let ty = Type::Function(func_ty, FunctionNature::Source(None));
				let ty = types.register_type(ty);
				Property::Value(ty)
			}
		}
	}
}

pub trait SynthesizableFunction {
	fn is_declare(&self) -> bool;

	fn is_async(&self) -> bool;

	fn get_set_generator_or_none(&self) -> GetSetGeneratorOrNone;

	fn id(&self) -> FunctionId;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn type_parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<GenericTypeParameters>;

	/// Has to be the first parameter
	fn this_constraint<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<TypeId>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> SynthesizedParameters;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	);

	fn return_type_annotation<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<(TypeId, Span)>;
}

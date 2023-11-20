use crate::{
	behavior::functions::ClassPropertiesToRegister, context::facts::Publicity, events::Event,
	synthesis::interfaces::GetterSetter, ASTImplementation, CheckingData, Environment,
	PropertyValue, TypeId,
};

use super::properties::PropertyKey;

// TODO better place
pub enum PropertyFunctionProperty {
	Get,
	Set,
	Standard { is_async: bool, is_generator: bool },
}

pub struct ClassValue<'a, M: ASTImplementation> {
	pub publicity: Publicity,
	/// Created eagerly, don't specialise
	pub key: PropertyKey<'static>,
	pub value: Option<&'a M::Expression>,
}

pub struct SynthesisedClassValue {
	pub publicity: Publicity,
	/// Created eagerly, don't specialise
	pub key: PropertyKey<'static>,
	pub effects: Vec<Event>,
	pub value: TypeId,
}

// TODO might use
// pub struct ClassMethod {
// 	publicity: Publicity,
// 	/// Created eagerly, don't specialise
// 	key: PropertyKey<'static>,
// 	value: PropertyFunctionProperty,
// }

/// TODO i really hate this setup. Can it be simpler & faster?
///
/// What about storing it as just set_events...?
pub struct RegisterClassPropertiesEvent {
	pub properties: Vec<SynthesisedClassValue>,
	pub class_prototype: TypeId,
}

fn register_properties_into_store<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	environment: &mut Environment,
	class_prototype: TypeId,
	properties: ClassPropertiesToRegister<'_, M>,
	checking_data: &mut CheckingData<T, M>,
) {
	let scope = crate::Scope::Function(crate::context::environment::FunctionScope::Constructor {
		extends: false,
		type_of_super: Some(TypeId::ERROR_TYPE),
		// TODO get from above
		this_object_type: TypeId::ERROR_TYPE,
	});

	let (_, result, _) = environment.new_lexical_environment_fold_into_parent(
		scope,
		checking_data,
		|environment, checking_data| {
			register_properties_into_environment(
				environment,
				class_prototype,
				checking_data,
				properties,
			);
		},
	);
	let (events, free_variables) = result.unwrap();

	// Store events ...
	todo!()
}

pub(crate) fn register_properties_into_environment<
	T: crate::ReadFromFS,
	M: crate::ASTImplementation,
>(
	environment: &mut Environment,
	on: TypeId,
	checking_data: &mut CheckingData<T, M>,
	properties: ClassPropertiesToRegister<M>,
) {
	for ClassValue { publicity, key, value } in properties.0 {
		let value = if let Some(expression) = value {
			PropertyValue::Value(M::synthesise_expression(
				expression,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			))
		} else {
			PropertyValue::Value(TypeId::UNDEFINED_TYPE)
		};
		environment.facts.register_property(on, publicity, key, value, true, None);
	}
}

use crate::{
	context::facts::PublicityKind, events::Event, synthesis::interfaces::GetterSetter,
	CheckingData, Environment, PropertyValue, TypeId,
};

use super::properties::PropertyKey;

// TODO better place
pub enum PropertyFunctionProperty {
	Get,
	Set,
	Standard { is_async: bool, is_generator: bool },
}

/// CreateProperty = get setter etc
pub enum PropertyOnClass<'a, M: crate::ASTImplementation> {
	Function { method: &'a M::ClassMethod, property: PropertyFunctionProperty },
	Expression(Option<&'a M::Expression>),
}

// Needed to because of enum
#[allow(type_alias_bounds)]
pub type ClassProperties<'a, M: crate::ASTImplementation> =
	Vec<(PublicityKind, PropertyKey<'static>, PropertyOnClass<'a, M>)>;

pub enum ClassProperty {
	Value {
		publicity: PublicityKind,
		/// Created eagerly, don't specialise
		key: PropertyKey<'static>,
		effects: Vec<Event>,
		end_value: PropertyFunctionProperty,
	},
	Method {
		publicity: PublicityKind,
		/// Created eagerly, don't specialise
		key: PropertyKey<'static>,
		value: PropertyFunctionProperty,
	},
}

/// TODO i really hate this setup. Can it be simpler & faster?
///
/// What about storing it as just set_events...?
pub struct RegisterClassPropertiesEvent {
	pub properties: Vec<ClassProperty>,
	pub class_prototype: TypeId,
}

fn register_properties_into_store<T: crate::ReadFromFS, M: crate::ASTImplementation>(
	environment: &mut Environment,
	class_prototype: TypeId,
	properties: Vec<(PublicityKind, PropertyKey<'static>, PropertyOnClass<'_, M>)>,
	checking_data: &mut CheckingData<T, M>,
) {
	let (_, result, _) = environment.new_lexical_environment_fold_into_parent(
		crate::Scope::Function(crate::context::environment::FunctionScope::Constructor {
			extends: false,
			type_of_super: Some(TypeId::ERROR_TYPE),
			// TODO get from above
			this_object_type: TypeId::ERROR_TYPE,
		}),
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
	properties: Vec<(PublicityKind, PropertyKey<'static>, PropertyOnClass<'_, M>)>,
) {
	for (publicity, property, value) in properties {
		let value = match value {
			PropertyOnClass::Function { method, property } => {
				let is_async = if let PropertyFunctionProperty::Standard { is_async, .. } = property
				{
					is_async
				} else {
					false
				};
				let is_generator =
					if let PropertyFunctionProperty::Standard { is_generator, .. } = property {
						is_generator
					} else {
						false
					};
				let getter_setter = match property {
					PropertyFunctionProperty::Get => GetterSetter::Getter,
					PropertyFunctionProperty::Set => GetterSetter::Setter,
					PropertyFunctionProperty::Standard { .. } => GetterSetter::None,
				};
				let behavior = crate::behavior::functions::FunctionRegisterBehavior::ClassMethod {
					is_async,
					is_generator,
					// TODO
					super_type: None,
				};
				let function = environment.new_function(checking_data, method, behavior);
				crate::behavior::functions::function_to_property(
					getter_setter,
					function,
					&mut checking_data.types,
				)
			}
			PropertyOnClass::Expression(Some(expression)) => PropertyValue::Value(
				M::synthesise_expression(expression, TypeId::ANY_TYPE, environment, checking_data),
			),
			PropertyOnClass::Expression(None) => PropertyValue::Value(TypeId::UNDEFINED_TYPE),
		};
		environment.facts.register_property(on, publicity, property, value, true, None);
	}
}

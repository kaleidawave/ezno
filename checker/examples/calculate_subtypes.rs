use ezno_checker::{
	features::objects::ObjectBuilder,
	subtyping::{type_is_subtype, type_is_subtype_object, State, SubTypingOptions},
	types::{
		generics::contributions::Contributions,
		properties::{PropertyKey, Publicity},
		TypeStore,
	},
	Constant, Environment, PropertyValue, RootContext, TypeId,
};
use source_map::Nullable;

fn main() {
	let root = RootContext::new_with_primitive_references();
	let mut environment = root.new_testing_context();
	let mut types = TypeStore::default();

	basics(&mut environment, &mut types);
	contributions(&mut environment, &mut types);
}

fn basics(environment: &mut Environment, types: &mut TypeStore) {
	let five = types.new_constant_type(Constant::Number(5f64.try_into().unwrap()));

	let string_or_number = types.new_or_type(TypeId::STRING_TYPE, TypeId::NUMBER_TYPE);

	eprintln!("--- basics ---");

	{
		let result = type_is_subtype_object(TypeId::NUMBER_TYPE, five, environment, types);

		eprintln!("number :> 5 {result:?}");
	}
	{
		let result =
			type_is_subtype_object(TypeId::NUMBER_TYPE, TypeId::STRING_TYPE, environment, types);

		eprintln!("number :> string {result:?}");
	}
	{
		let result =
			type_is_subtype_object(string_or_number, TypeId::STRING_TYPE, environment, types);

		eprintln!("string | number :> string {result:?}");
	}

	eprintln!("--------------\n");
}

fn contributions(environment: &mut Environment, types: &mut TypeStore) {
	// TODO types API, which doesn't is less hand-holdy
	let generic_parameter =
		environment.new_explicit_type_parameter("T", Some(TypeId::NUMBER_TYPE), None, types);

	// create `{}` and add `inner: T`
	let object = types.new_anonymous_interface_type();
	let inner = PropertyKey::String(std::borrow::Cow::Owned("inner".to_owned()));
	environment.info.register_property(
		object,
		Publicity::Public,
		inner.clone(),
		PropertyValue::Value(generic_parameter.type_id),
		false,
		source_map::SpanWithSource::NULL,
	);

	let or = types.new_or_type(generic_parameter.type_id, object);
	let parameter = types.new_function_parameter(or);

	let five = types.new_constant_type(Constant::Number(5f64.try_into().unwrap()));

	let five_obj = {
		let mut basis = ObjectBuilder::new(
			None,
			types,
			source_map::SpanWithSource::NULL,
			&mut environment.info,
		);
		basis.append(
			environment,
			Publicity::Public,
			inner,
			PropertyValue::Value(five),
			source_map::SpanWithSource::NULL,
		);
		basis.build_object()
	};

	// x(5)
	{
		let contributions = Contributions::default();
		let mut state = State {
			already_checked: Default::default(),
			mode: Default::default(),
			contributions: Some(contributions),
			others: SubTypingOptions::default(),
			object_constraints: None,
		};
		let result = type_is_subtype(parameter, five_obj, &mut state, environment, types);

		let contributions = state.contributions.as_ref().unwrap();

		eprintln!(
			"{:?}:>{:?}
result={:?}
staging_covariant={:?}
staging_contravariant={:?}",
			parameter,
			five,
			result,
			contributions.staging_covariant,
			contributions.staging_contravariant
		);
	}
}

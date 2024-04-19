#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{
		subtyping::type_is_subtype, synthesis::EznoParser, types::BasicEquality, ASTImplementation,
		CheckingData, RootContext, TypeCheckOptions, TypeId,
	};

	let mut checking_data = CheckingData::<_, EznoParser>::new(
		TypeCheckOptions::default(),
		&|_: &std::path::Path| None::<Vec<u8>>,
		None,
		<EznoParser as ASTImplementation>::ParserRequirements::default(),
	);

	let root = RootContext::new_with_primitive_references();

	let mut environment = root.new_testing_context();

	let mut behavior = BasicEquality::default();

	let five = checking_data
		.types
		.new_constant_type(ezno_checker::Constant::Number(5f64.try_into().unwrap()));

	let string_or_number =
		checking_data.types.new_or_type(TypeId::STRING_TYPE, TypeId::NUMBER_TYPE);

	{
		let result = type_is_subtype(
			TypeId::NUMBER_TYPE,
			five,
			&mut behavior,
			&mut environment,
			&checking_data.types,
		);

		eprintln!("number :> 5 {result:?}")
	}
	{
		let result = type_is_subtype(
			TypeId::NUMBER_TYPE,
			TypeId::STRING_TYPE,
			&mut behavior,
			&mut environment,
			&checking_data.types,
		);

		eprintln!("number :> string {result:?}")
	}
	{
		let result = type_is_subtype(
			string_or_number,
			TypeId::STRING_TYPE,
			&mut behavior,
			&mut environment,
			&checking_data.types,
		);

		eprintln!("string | number :> string {result:?}")
	}
}

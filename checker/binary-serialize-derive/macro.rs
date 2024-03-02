use proc_macro::TokenStream;
use syn_helpers::{
	derive_trait,
	proc_macro2::Span,
	syn::{parse_macro_input, parse_quote, DeriveInput, Expr, Ident, Stmt},
	Constructable, FieldMut, NamedOrUnnamedFieldMut, Structure, Trait, TraitItem, TypeOfSelf,
};

#[proc_macro_derive(BinarySerializable)]
pub fn derive_binary_serializable(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let result = derive_trait(
		input,
		Trait {
			name: parse_quote!(crate::BinarySerializable),
			generic_parameters: None,
			items: vec![
				TraitItem::new_method(
					Ident::new("serialize", Span::call_site()),
					None,
					TypeOfSelf::Owned,
					vec![parse_quote!(buf: &mut Vec<u8>)],
					None,
					|mut item| {
						item.map_constructable(|mut constructable| {
							let iterator = constructable
								.as_enum_variant()
								.map(|variant| {
									let idx = variant.idx as u8;
									parse_quote!(buf.push(#idx);)
								})
								.into_iter()
								.chain(constructable.get_fields_mut().fields_iterator_mut().map(
									|mut field: NamedOrUnnamedFieldMut| -> Stmt {
										let reference = field.get_reference();
										parse_quote!(crate::BinarySerializable::serialize(#reference, buf);)
									},
								));
							Ok(iterator.collect())
						})
					},
				),
				TraitItem::new_associated_function(
					Ident::new("deserialize", Span::call_site()),
					Some(vec![parse_quote!(I: Iterator<Item = u8>)]),
					vec![
						parse_quote!(iter: &mut I),
						parse_quote!(backing_source: ::source_map::SourceId),
					],
					Some(parse_quote!(Self)),
					|structure| {
						let deserialize_call: Expr = parse_quote!(
							crate::BinarySerializable::deserialize(iter, backing_source)
						);

						match structure {
							Structure::Enum(r#enum) => {
								let indexer: Stmt =
									parse_quote!(let indexer = iter.next().unwrap(););

								let bad_case = parse_quote!(
									unreachable!("invalid discriminant when deserializing enum");
								);

								Ok(std::iter::once(indexer)
									.chain(r#enum.get_variants().iter().map(|variant| {
										let idx = variant.idx as u8;
										let constructor = variant
											.build_constructor(|_| Ok(deserialize_call.clone()))
											.unwrap();

										parse_quote!(if indexer == #idx {
											return #constructor;
										})
									}))
									.chain(std::iter::once(bad_case))
									.collect())
							}
							Structure::Struct(r#struct) => r#struct
								.build_constructor(|_| Ok(deserialize_call.clone()))
								.map(|expr| vec![Stmt::Expr(expr, None)]),
						}
					},
				),
			],
		},
	);

	result.into()
}

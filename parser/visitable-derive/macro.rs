use std::error::Error;

use proc_macro::TokenStream;
use syn_helpers::{
	derive_trait,
	proc_macro2::{Ident, Span},
	quote,
	syn::{parse_macro_input, parse_quote, DeriveInput, Stmt, __private::quote::format_ident},
	Constructable, FieldMut, HasAttributes, NamedOrUnnamedFieldMut, Trait, TraitItem,
};

/// On the top structure
const VISIT_SELF_NAME: &str = "visit_self";
/// Per field modifiers
const VISIT_SKIP_NAME: &str = "visit_skip_field";
/// Add to chain. Can be on item or a field
const VISIT_WITH_CHAIN_NAME: &str = "visit_with_chain";

/// Usage #[derive(Visitable)]
#[proc_macro_derive(Visitable, attributes(visit_self, visit_skip_field, visit_custom_visit))]
pub fn generate_visit_implementation(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let visit_item = TraitItem::new_method(
		Ident::new("visit", Span::call_site()),
		Some(vec![parse_quote!(TData)]),
		syn_helpers::TypeOfSelf::Reference,
		vec![
			parse_quote!(visitors: &mut (impl crate::visiting::VisitorReceiver<TData> + ?Sized)),
			parse_quote!(data: &mut TData),
			parse_quote!(settings: &crate::VisitSettings),
			parse_quote!(functions: &mut crate::ExtractedFunctions),
			parse_quote!(chain: &mut ::temporary_annex::Annex<crate::visiting::Chain>),
		],
		None,
		|item| generated_visit_item(item, VisitType::Immutable),
	);

	let visit_mut_item = TraitItem::new_method(
		Ident::new("visit_mut", Span::call_site()),
		Some(vec![parse_quote!(TData)]),
		syn_helpers::TypeOfSelf::MutableReference,
		vec![
			parse_quote!(visitors: &mut (impl crate::visiting::VisitorMutReceiver<TData> + ?Sized)),
			parse_quote!(data: &mut TData),
			parse_quote!(settings: &crate::VisitSettings),
			parse_quote!(functions: &mut crate::ExtractedFunctions),
			parse_quote!(chain: &mut ::temporary_annex::Annex<crate::visiting::Chain>),
		],
		None,
		|item| generated_visit_item(item, VisitType::Mutable),
	);

	let visitable_trait = Trait {
		name: parse_quote!(crate::visiting::Visitable),
		generic_parameters: None,
		items: vec![visit_item, visit_mut_item],
	};

	let output = derive_trait(input, visitable_trait);

	output.into()
}

#[derive(Clone, Copy)]
enum VisitType {
	Immutable,
	Mutable,
}

fn generated_visit_item(
	mut item: syn_helpers::Item,
	visit_type: VisitType,
) -> Result<Vec<Stmt>, Box<dyn Error>> {
	let attributes = item.structure.get_attributes();

	let visit_self = attributes.iter().any(|attr| attr.path.is_ident(VISIT_SELF_NAME));

	let visit_with_chain = attributes
		.iter()
		.find_map(|attr| attr.path.is_ident(VISIT_WITH_CHAIN_NAME).then_some(&attr.tokens));

	let mut lines = Vec::new();

	if let Some(expr_tokens) = visit_with_chain {
		lines.push(parse_quote!( let mut chain = &mut chain.push_annex(#expr_tokens); ))
	}

	if visit_self {
		let struct_name_as_snake_case = str_to_snake_case(&item.structure.get_name().to_string());
		let mut_postfix =
			matches!(visit_type, VisitType::Mutable).then_some("_mut").unwrap_or_default();
		let func_name = format_ident!("visit_{}{}", struct_name_as_snake_case, mut_postfix);

		lines.push(parse_quote!( visitors.#func_name(self, data, functions, chain); ))
	}

	let mut field_lines = item.map_constructable(|mut constructable| {
		Ok(constructable
			.get_fields_mut()
			.fields_iterator_mut()
			.flat_map(|mut field: NamedOrUnnamedFieldMut| -> Option<Stmt> {
				let attributes = field.get_attributes();

				let skip_field = attributes.iter().any(|attr| attr.path.is_ident(VISIT_SKIP_NAME));

				let visit_with_chain = attributes.iter().find_map(|attr| {
					attr.path.is_ident(VISIT_WITH_CHAIN_NAME).then_some(&attr.tokens)
				});

				let chain = if let Some(expr_tokens) = visit_with_chain {
					quote!(&mut chain.push_annex(#expr_tokens))
				} else {
					quote!(chain)
				};

				if !skip_field {
					let reference = field.get_reference();
					Some(match visit_type {
						VisitType::Immutable => parse_quote! {
							crate::Visitable::visit(#reference, visitors, data, settings, functions, #chain);
						},
						VisitType::Mutable => parse_quote! {
							crate::Visitable::visit_mut(#reference, visitors, data, settings, functions, #chain);
						},
					})
				} else {
					None
				}
			})
			.collect::<Vec<_>>())
	})?;

	lines.append(&mut field_lines);

	Ok(lines)
}

fn str_to_snake_case(s: &str) -> String {
	fn is_divider(c: char) -> bool {
		c.is_uppercase() || c.is_numeric() || c == '_'
	}

	let mut peekable = s.chars().peekable();
	let mut string: String = peekable.next().unwrap().to_lowercase().collect();
	while let Some(char) = peekable.next() {
		if let Some(next) = peekable.peek() {
			if next.is_lowercase() && is_divider(char) {
				string.push('_');
			}
			string.extend(char.to_lowercase());
		} else {
			string.push(char);
		}
	}

	string
}

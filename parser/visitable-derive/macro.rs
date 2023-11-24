use proc_macro::TokenStream;
use std::error::Error;
use string_cases::StringCasesExt;
use syn_helpers::{
	derive_trait,
	proc_macro2::{Ident, Span, TokenTree},
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
			parse_quote!(options: &crate::VisitOptions),
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
			parse_quote!(options: &crate::VisitOptions),
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

	let visit_self = attributes
		.iter()
		.find_map(|attr| attr.path.is_ident(VISIT_SELF_NAME).then_some(&attr.tokens));

	let visit_with_chain = attributes
		.iter()
		.find_map(|attr| attr.path.is_ident(VISIT_WITH_CHAIN_NAME).then_some(&attr.tokens));

	let mut lines = Vec::new();

	if let Some(expr_tokens) = visit_with_chain {
		lines.push(parse_quote!( let mut chain = &mut chain.push_annex(#expr_tokens); ))
	}

	if let Some(tokens) = visit_self.cloned() {
		let mut under = None;
		if let Some(TokenTree::Group(g)) = tokens.into_iter().next() {
			let mut tokens = g.stream().into_iter();
			if let Some(TokenTree::Ident(ident)) = tokens.next() {
				if ident == "under" {
					if let Some(TokenTree::Ident(literal)) = tokens.next() {
						under = Some(literal.to_string());
					}
				}
			}
			// TODO error
		}

		let mut_postfix =
			matches!(visit_type, VisitType::Mutable).then_some("_mut").unwrap_or_default();
		if let Some(under) = under {
			let func_name = format_ident!("visit_{}{}", under, mut_postfix);
			lines.push(parse_quote!(visitors.#func_name(self.into(), data,  chain); ))
		} else {
			let struct_name_as_snake_case = &item.structure.get_name().to_string().to_snake_case();
			let func_name = format_ident!("visit_{}{}", struct_name_as_snake_case, mut_postfix);
			lines.push(parse_quote!( visitors.#func_name(self, data,  chain); ))
		}
	}

	let mut field_lines = item.map_constructable(|mut constructable| {
		Ok(constructable
			.get_fields_mut()
			.fields_iterator_mut()
			.flat_map(|mut field: NamedOrUnnamedFieldMut| -> Option<Stmt> {
				let attributes = field.get_attributes();

				let skip_field_attr =
					attributes.iter().find(|attr| attr.path.is_ident(VISIT_SKIP_NAME));

				// TODO maybe?
				// // None == unconditional
				// let _skip_field_expression: Option<Expr> =
				// 	skip_field_attr.as_ref().map(|attr| attr.bracket_token);

				let visit_with_chain = attributes.iter().find_map(|attr| {
					attr.path.is_ident(VISIT_WITH_CHAIN_NAME).then_some(&attr.tokens)
				});

				let chain = if let Some(expr_tokens) = visit_with_chain {
					quote!(&mut chain.push_annex(#expr_tokens))
				} else {
					quote!(chain)
				};

				if skip_field_attr.is_none() {
					let reference = field.get_reference();
					Some(match visit_type {
						VisitType::Immutable => parse_quote! {
							crate::Visitable::visit(#reference, visitors, data, options, #chain);
						},
						VisitType::Mutable => parse_quote! {
							crate::Visitable::visit_mut(#reference, visitors, data, options, #chain);
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

use proc_macro::{token_stream, Delimiter, Spacing, TokenStream, TokenTree};
use proc_macro2::Span;
use quote::{format_ident, quote};

/// Used for generating parser::ASTNodes using proc macros
///
/// Turns token stream into string.
/// - Finds expressions and registers cursor locations
/// - Parses structure from string and turns it into Rust tokens
#[proc_macro]
pub fn expr(item: TokenStream) -> TokenStream {
	token_stream_to_ast_node::<parser::Expression>(item)
}

#[proc_macro]
pub fn stmt(item: TokenStream) -> TokenStream {
	token_stream_to_ast_node::<parser::StatementOrDeclaration>(item)
}

struct InterpolationPoint {
	position: usize,
	expr_name: String,
}

fn token_stream_to_ast_node<T: parser::ASTNode + self_rust_tokenize::SelfRustTokenize>(
	item: TokenStream,
) -> TokenStream {
	let mut cursor_locations = Vec::new();
	let mut string = String::new();
	parse_token_stream(item.into_iter(), &mut string, &mut cursor_locations);

	let cursors = cursor_locations
		.iter()
		.enumerate()
		.map(|(idx, InterpolationPoint { position, expr_name: _ })| {
			(
				*position,
				parser::CursorId(idx.try_into().unwrap(), std::marker::PhantomData::default()),
			)
		})
		.collect();

	let parse_result = T::from_string(
		string,
		parser::ParseSettings::default(),
		parser::SourceId::NULL,
		None,
		cursors,
	);

	let node = match parse_result {
		Ok(node) => node,
		Err(err) => {
			let reason = err.reason;
			return quote!(compile_error!(#reason)).into();
		}
	};

	let node_as_tokens = self_rust_tokenize::SelfRustTokenize::to_tokens(&node);

	let interpolation_tokens = cursor_locations.iter().enumerate().map(
		|(idx, InterpolationPoint { position: _, expr_name })| {
			let ident = format_ident!("_cursor_{idx}");
			let expr_ident = proc_macro2::Ident::new(expr_name, Span::call_site());
			quote!(let #ident = #expr_ident)
		},
	);

	let tokens = quote! {
		{
			use parser::{ast::*, Span, SourceId};
			#(#interpolation_tokens;)*
			const CURRENT_SOURCE_ID: SourceId = SourceId::NULL;
			#node_as_tokens
		}
	};

	// eprintln!("{tokens}");

	tokens.into()
}

fn parse_token_stream(
	mut token_iter: token_stream::IntoIter,
	string: &mut String,
	cursor_locations: &mut Vec<InterpolationPoint>,
) {
	let mut last_was_ident = false;
	while let Some(token_tree) = token_iter.next() {
		let current_is_ident = matches!(token_tree, TokenTree::Ident(_));

		match token_tree {
			TokenTree::Group(group) => {
				let delimiter = group.delimiter();
				let (start, end) = match delimiter {
					Delimiter::Parenthesis => ("(", ")"),
					Delimiter::Brace => ("{", "}"),
					Delimiter::Bracket => ("[", "]"),
					Delimiter::None => ("", ""),
				};
				string.push_str(start);
				parse_token_stream(group.stream().into_iter(), string, cursor_locations);
				string.push_str(end);
			}
			TokenTree::Ident(ident) => {
				if last_was_ident {
					string.push(' ');
				}
				string.push_str(ident.to_string().as_str());
			}
			TokenTree::Punct(punctuation) => {
				let chr = punctuation.as_char();
				if chr == '#' {
					if let Some(TokenTree::Ident(ident)) = token_iter.next() {
						let expr_name = ident.to_string();
						cursor_locations
							.push(InterpolationPoint { position: string.len(), expr_name });
					} else {
						panic!("Expected ident")
					}
				} else {
					let spacing = matches!(punctuation.spacing(), Spacing::Alone);
					if spacing && !(string.ends_with("<") && chr == '/') {
						string.push(' ');
					}
					string.push(chr);
					if spacing {
						string.push(' ');
					}
				}
			}
			TokenTree::Literal(literal) => {
				string.push_str(literal.to_string().as_str());
			}
		}

		last_was_ident = current_is_ident;
	}
}

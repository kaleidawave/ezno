use proc_macro::{token_stream, Delimiter, Spacing, TokenStream, TokenTree};
use proc_macro2::Span;
use quote::{format_ident, quote};

/// Used for generating ezno_parser::ASTNodes using proc macros
///
/// Turns token stream into string.
/// - Finds expressions and registers marker locations
/// - Parses structure from string and turns it into Rust tokens
#[proc_macro]
pub fn expr(item: TokenStream) -> TokenStream {
	token_stream_to_ast_node::<ezno_parser::Expression>(item)
}

#[proc_macro]
pub fn stmt(item: TokenStream) -> TokenStream {
	token_stream_to_ast_node::<ezno_parser::StatementOrDeclaration>(item)
}

fn token_stream_to_ast_node<T: ezno_parser::ASTNode + self_rust_tokenize::SelfRustTokenize>(
	item: TokenStream,
) -> TokenStream {
	let mut string_to_parse = String::new();
	let mut marker_items = Vec::new();
	parse_token_stream(item.into_iter(), &mut string_to_parse, &mut marker_items);

	// TODO can you get new lines in macro?
	let line_starts = ezno_parser::source_map::LineStarts::new("");
	let options = ezno_parser::ParseOptions { interpolation_points: true, ..Default::default() };
	let parse_result =
		ezno_parser::lex_and_parse_script::<T>(line_starts, options, &string_to_parse, None);

	let node = match parse_result {
		Ok((node, _state)) => node,
		Err(err) => {
			let reason = err.reason;
			return quote!(compile_error!(#reason)).into();
		}
	};

	let node_as_tokens = self_rust_tokenize::SelfRustTokenize::to_tokens(&node);

	let interpolation_tokens = marker_items.iter().enumerate().map(|(idx, name)| {
		let ident = format_ident!("_marker_{idx}");
		let expr_ident = proc_macro2::Ident::new(name, Span::call_site());
		quote!(let #ident = #expr_ident)
	});

	let tokens = quote! {
		{
			use ezno_parser::ast::*;
			use ezno_parser::generator_helpers::IntoAST;

			#(#interpolation_tokens;)*
			const CURRENT_SOURCE_ID: SourceId = SourceId::NULL;
			#node_as_tokens
		}
	};

	// eprintln!("output: {tokens}");

	tokens.into()
}

fn parse_token_stream(
	mut token_iter: token_stream::IntoIter,
	string: &mut String,
	marker_items: &mut Vec<String>,
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
				parse_token_stream(group.stream().into_iter(), string, marker_items);
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
						marker_items.push(expr_name);
					} else {
						panic!("Expected ident")
					}
					string.push('\u{03A9}');
				} else {
					let spacing = matches!(punctuation.spacing(), Spacing::Alone)
						&& !matches!(chr, '<' | '>' | '/');
					if spacing && !string.ends_with("</") {
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

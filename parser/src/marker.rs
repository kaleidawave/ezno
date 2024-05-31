use std::marker::PhantomData;

/// Places in the AST which
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Marker<T>(pub u8, pub PhantomData<T>);

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const TYPES: &str = r"
	type Marker<T> = number;
";

pub const MARKER: &str = "EZNO_GENERATOR_SLOT";

#[cfg(feature = "serde-serialize")]
impl<T> serde::Serialize for Marker<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		self.0.serialize(serializer)
	}
}

// Custom implementation used by the generator to interpolate nodes
#[cfg(feature = "self-rust-tokenize")]
impl<T> self_rust_tokenize::SelfRustTokenize for Marker<T> {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		use self_rust_tokenize::proc_macro2::{Ident, Span};
		let token = Ident::new(&format!("_marker_{}", self.0), Span::call_site());
		token_stream.extend(self_rust_tokenize::quote!(ezno_parser::IntoAST::into_ast(#token)));
	}
}

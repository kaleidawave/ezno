use std::marker::PhantomData;

pub type EmptyCursorId = CursorId<()>;

impl EmptyCursorId {
	#[must_use]
	pub fn new(id: u8) -> Self {
		Self(id, PhantomData)
	}

	pub(crate) fn into_cursor<T>(self) -> CursorId<T> {
		CursorId(self.0, PhantomData)
	}
}

/// A cursor
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CursorId<T>(pub u8, pub PhantomData<T>);

#[cfg(feature = "serde-serialize")]
impl<T> serde::Serialize for CursorId<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		self.0.serialize(serializer)
	}
}

// Custom implementation used by the generator to interpolate nodes
#[cfg(feature = "self-rust-tokenize")]
impl<T> self_rust_tokenize::SelfRustTokenize for CursorId<T> {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		use self_rust_tokenize::proc_macro2::{Ident, Span};
		let token = Ident::new(&format!("_cursor_{}", self.0), Span::call_site());
		token_stream.extend(self_rust_tokenize::quote!(ezno_parser::IntoAST::into_ast(#token)));
	}
}

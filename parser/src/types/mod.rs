pub mod type_declarations;
pub mod type_references;

use std::sync::atomic::{AtomicU16, Ordering};

use derive_debug_extras::DebugExtras;

static TYPE_COUNTER: AtomicU16 = AtomicU16::new(1);

/// A id of a syntax types
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(u16);

impl TypeId {
	pub fn new() -> Self {
		TypeId(TYPE_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	pub const fn new_from_id(id: u16) -> Self {
		TypeId(id)
	}

	#[doc(hidden)]
	pub fn unwrap_identifier(self) -> u16 {
		self.0
	}

	pub const NULL: Self = Self(0);
}

// TODO not sure
#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for TypeId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(TypeId::new()))
	}
}

// [See](https://www.typescriptlang.org/docs/handbook/2/classes.html#member-visibility)
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Visibility {
// 	Private,
// 	Public,
// 	Protected,
// }

// impl Visibility {
// 	pub fn as_str(&self) -> &'static str {
// 		match self {
// 			Visibility::Private => "private ",
// 			Visibility::Public => "public ",
// 			Visibility::Protected => "protected ",
// 		}
// 	}
// }

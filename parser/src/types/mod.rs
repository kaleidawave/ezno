//! Includes type annotations + syntax added by TypeScript (and Ezno) such as `declare` declarations

pub mod declare_variable;
pub mod enum_declaration;
pub mod interface;
pub mod namespace;
pub mod type_alias;
pub mod type_annotations;
pub mod type_declarations;

pub use interface::InterfaceDeclaration;
pub use type_annotations::TypeAnnotation;

use crate::derive_ASTNode;

/// [See](https://www.typescriptlang.org/docs/handbook/2/classes.html#member-visibility)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
	Private,
	Public,
	Protected,
}

impl Visibility {
	#[must_use]
	pub fn as_str(&self) -> &'static str {
		match self {
			Visibility::Private => "private ",
			Visibility::Public => "public ",
			Visibility::Protected => "protected ",
		}
	}

	// #[must_use]
	// pub fn token_is_visibility_specifier(t: &TSXToken) -> bool {
	// matches!(
	// 	t,
	// 	TSXToken::Keyword(TSXKeyword::Private | TSXKeyword::Public | TSXKeyword::Protected)
	// )
	// }
}

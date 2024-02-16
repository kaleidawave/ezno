//! Includes type annotations + syntax added by TypeScript (and Ezno) such as `declare` declarations

pub mod declares;
pub mod enum_declaration;
pub mod interface;
pub mod namespace;
pub mod type_alias;
pub mod type_annotations;
pub mod type_declarations;

pub use interface::InterfaceDeclaration;

use crate::{derive_ASTNode, TSXKeyword, TSXToken};

/// [See](https://www.typescriptlang.org/docs/handbook/2/classes.html#member-visibility)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
	Private,
	Public,
	Protected,
}

impl Visibility {
	pub fn as_str(&self) -> &'static str {
		match self {
			Visibility::Private => "private ",
			Visibility::Public => "public ",
			Visibility::Protected => "protected ",
		}
	}

	pub fn token_is_visibility_specifier(t: &TSXToken) -> bool {
		matches!(
			t,
			TSXToken::Keyword(TSXKeyword::Private | TSXKeyword::Public | TSXKeyword::Protected)
		)
	}
}

#[cfg(feature = "extras")]
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum AnnotationPerforms {
	PerformsStatements { body: crate::Block },
	PerformsConst { identifier: String },
}

#[cfg(feature = "extras")]
impl crate::ASTNode for AnnotationPerforms {
	fn get_position(&self) -> &source_map::Span {
		todo!()
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let _ = reader.expect_next(crate::TSXToken::Keyword(crate::TSXKeyword::Performs))?;
		if let Some(tokenizer_lib::Token(crate::TSXToken::OpenBrace, _)) = reader.peek() {
			// let expression = Expression::from_reader(reader, state, options)?;
			// reader.expect_next(TSXToken::CloseParentheses)?;
			// Some(Box::new(expression))

			let body = crate::Block::from_reader(reader, state, options)?;
			Ok(AnnotationPerforms::PerformsStatements { body })
		} else {
			reader.expect_next(crate::TSXToken::Keyword(crate::TSXKeyword::Const))?;
			let (identifier, _) =
				crate::tokens::token_as_identifier(reader.next().unwrap(), "performs const")?;
			Ok(AnnotationPerforms::PerformsConst { identifier })
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		todo!()
	}
}

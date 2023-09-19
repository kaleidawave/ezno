pub mod declares;
pub mod enum_declaration;
pub mod interface;
pub mod namespace;
pub mod type_alias;
pub mod type_annotations;
pub mod type_declarations;

pub use interface::InterfaceDeclaration;
use source_map::Span;

use crate::{tsx_keywords, ASTNode, Block, Keyword};

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

#[cfg(feature = "extras")]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum AnnotationPerforms {
	PerformsStatements { performs_keyword: Keyword<tsx_keywords::Performs>, statements: Block },
	PerformsConst { performs_keyword: Keyword<tsx_keywords::Performs>, identifier: String },
}

#[cfg(feature = "extras")]
impl ASTNode for AnnotationPerforms {
	fn get_position(&self) -> &Span {
		todo!()
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> crate::ParseResult<Self> {
		let performs_keyword = Keyword::from_reader(reader)?;
		if let Some(tokenizer_lib::Token(crate::TSXToken::OpenBrace, _)) = reader.peek() {
			// let expression = Expression::from_reader(reader, state, settings)?;
			// reader.expect_next(TSXToken::CloseParentheses)?;
			// Some(Box::new(expression))

			let body = Block::from_reader(reader, state, settings)?;
			Ok(AnnotationPerforms::PerformsStatements { performs_keyword, statements: body })
		} else {
			reader.expect_next(crate::TSXToken::Keyword(crate::TSXKeyword::Const))?;
			let (identifier, _) =
				crate::tokens::token_as_identifier(reader.next().unwrap(), "performs const")?;
			Ok(AnnotationPerforms::PerformsConst { performs_keyword, identifier })
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		todo!()
	}
}

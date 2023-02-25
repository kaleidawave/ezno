use std::borrow::Cow;

use crate::tsx_keywords;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};
use visitable_derive::Visitable;

use crate::{errors::parse_lexing_error, ASTNode, Block, Keyword, TSXKeyword, TSXToken};

pub enum CustomBlockKeywords {
	Server(Keyword<tsx_keywords::Server>),
	Module(Keyword<tsx_keywords::Module>),
}

/// A block which contains code that "should" go on the server
/// TODO visitable should do be custom here to add thing to chain
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
pub enum CustomBlock {
	ServerBlock {
		keyword: Keyword<tsx_keywords::Server>,
		block: Block,
	},
	/// https://github.com/tc39/proposal-js-module-blocks
	ModuleBlock {
		keyword: Keyword<tsx_keywords::Module>,
		block: Block,
	},
}

#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for CustomBlock {
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!()
	}
}

impl ASTNode for CustomBlock {
	fn get_position(&self) -> Cow<Span> {
		todo!()
		// match self {
		// 	CustomBlock::ServerBlock { position, .. }
		// 	| CustomBlock::ModuleBlock { position, .. }
		// 	| CustomBlock::DoBlock { position, .. } => position.as_ref(),
		// }
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, source_map::Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> Result<Self, crate::ParseError> {
		let Token(token, span) = reader.next().ok_or_else(parse_lexing_error)?;
		let keyword = match token {
			TSXToken::Keyword(TSXKeyword::Server) => {
				CustomBlockKeywords::Server(Keyword::new(span))
			}
			TSXToken::Keyword(TSXKeyword::Module) => {
				CustomBlockKeywords::Module(Keyword::new(span))
			}
			_token => todo!(),
		};
		Self::from_reader_with_keyword(reader, state, settings, keyword)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
		_depth: u8,
	) {
		todo!()
		// buf.push_str("server ");
		// self.block.to_string_from_buffer(buf, settings, depth);
	}
}

impl CustomBlock {
	pub(crate) fn from_reader_with_keyword(
		reader: &mut impl TokenReader<TSXToken, source_map::Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
		keyword: CustomBlockKeywords,
	) -> Result<Self, crate::ParseError> {
		let block = Block::from_reader(reader, state, settings)?;
		Ok(match keyword {
			CustomBlockKeywords::Server(keyword) => Self::ServerBlock { keyword, block },
			CustomBlockKeywords::Module(keyword) => Self::ModuleBlock { keyword, block },
		})
	}
}

use std::{
	borrow::Cow,
	sync::atomic::{AtomicU16, Ordering},
};

use derive_debug_extras::DebugExtras;
use derive_enum_from_into::EnumFrom;
use visitable_derive::Visitable;

use super::{ASTNode, Span, TSXToken, TokenReader};
use crate::{
	extractor::ExtractedFunctions,
	statements::{parse_statements, statements_to_string},
	ParseResult, ParseSettings, Statement, VisitSettings, Visitable,
};

static BLOCK_ID_COUNTER: AtomicU16 = AtomicU16::new(0);

/// A identifier for a group of statements
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct BlockId(u16);

// TODO not sure
#[cfg(feature = "self-rust-tokenize")]
impl self_rust_tokenize::SelfRustTokenize for BlockId {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		token_stream.extend(self_rust_tokenize::quote!(BlockId::new()))
	}
}

impl BlockId {
	pub fn new() -> Self {
		Self(BLOCK_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
	}

	/// TODO temp
	pub fn unwrap_counter(&self) -> u16 {
		self.0
	}
}

/// A "block" of braced statements
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct Block(pub Vec<Statement>, pub BlockId, pub Span);

impl Eq for Block {}

impl PartialEq for Block {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}

pub struct BlockLike<'a> {
	pub block_id: BlockId,
	pub statements: &'a Vec<Statement>,
}

pub struct BlockLikeMut<'a> {
	pub block_id: BlockId,
	pub statements: &'a mut Vec<Statement>,
}

impl<'a> From<&'a Block> for BlockLike<'a> {
	fn from(block: &'a Block) -> Self {
		BlockLike { block_id: block.1, statements: &block.0 }
	}
}

impl<'a> From<&'a mut Block> for BlockLikeMut<'a> {
	fn from(block: &'a mut Block) -> Self {
		BlockLikeMut { block_id: block.1, statements: &mut block.0 }
	}
}

impl ASTNode for Block {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start_span = reader.expect_next(TSXToken::OpenBrace)?;
		let (statements, block_id) = parse_statements(reader, state, settings)?;
		let end_span = reader.expect_next(TSXToken::CloseBrace)?;
		Ok(Self(statements, block_id, start_span.union(&end_span)))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		buf.push('{');
		if depth > 0 {
			buf.push_new_line();
		}
		statements_to_string(&self.0, buf, settings, depth);
		buf.push_new_line();
		if depth > 1 {
			settings.0.add_indent(depth - 1, buf);
		}
		buf.push('}');
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.2)
	}
}

impl Block {
	pub fn iter(&self) -> core::slice::Iter<'_, Statement> {
		self.0.iter()
	}

	pub fn iter_mut(&mut self) -> core::slice::IterMut<'_, Statement> {
		self.0.iter_mut()
	}
}

impl Visitable for Block {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		{
			visitors.visit_block(
				&crate::block::BlockLike { block_id: self.1, statements: &self.0 },
				data,
				functions,
				chain,
			);
		}
		let iter = self.0.iter();
		if settings.reverse_statements {
			iter.rev()
				.for_each(|statement| statement.visit(visitors, data, settings, functions, chain));
		} else {
			iter.for_each(|statement| statement.visit(visitors, data, settings, functions, chain));
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut { block_id: self.1, statements: &mut self.0 },
				data,
				functions,
				chain,
			);
		}
		let iter_mut = self.0.iter_mut();
		if settings.reverse_statements {
			iter_mut.for_each(|statement| {
				statement.visit_mut(visitors, data, settings, functions, chain)
			});
		} else {
			iter_mut.rev().for_each(|statement| {
				statement.visit_mut(visitors, data, settings, functions, chain)
			});
		}
	}
}

/// For ifs and other statements
#[derive(Debug, Clone, PartialEq, Eq, Visitable, EnumFrom)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum BlockOrSingleStatement {
	Braced(Block),
	SingleStatement(Box<Statement>),
}

impl From<Statement> for BlockOrSingleStatement {
	fn from(stmt: Statement) -> Self {
		Self::SingleStatement(Box::new(stmt))
	}
}

impl ASTNode for BlockOrSingleStatement {
	fn get_position(&self) -> Cow<Span> {
		match self {
			BlockOrSingleStatement::Braced(blk) => blk.get_position(),
			BlockOrSingleStatement::SingleStatement(stmt) => stmt.get_position(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		Statement::from_reader(reader, state, settings).map(|stmt| match stmt {
			Statement::Block(blk) => Self::Braced(blk),
			stmt => Box::new(stmt).into(),
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			BlockOrSingleStatement::Braced(block) => {
				block.to_string_from_buffer(buf, settings, depth)
			}
			BlockOrSingleStatement::SingleStatement(stmt) => {
				stmt.to_string_from_buffer(buf, settings, depth)
			}
		}
	}
}

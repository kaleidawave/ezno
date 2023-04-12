use std::{
	borrow::Cow,
	sync::atomic::{AtomicU16, Ordering},
};

use derive_debug_extras::DebugExtras;
use derive_enum_from_into::EnumFrom;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::Token;
use visitable_derive::Visitable;

use super::{ASTNode, Span, TSXToken, TokenReader};
use crate::{
	expect_semi_colon, Declaration, ParseResult, ParseSettings, Statement, VisitSettings, Visitable,
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

#[derive(Debug, Clone, PartialEq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum StatementOrDeclaration {
	Statement(Statement),
	Declaration(Declaration),
}

impl StatementOrDeclaration {
	pub(crate) fn requires_semi_colon(&self) -> bool {
		match self {
			StatementOrDeclaration::Statement(stmt) => stmt.requires_semi_colon(),
			StatementOrDeclaration::Declaration(dec) => matches!(
				dec,
				Declaration::Variable(..) | Declaration::Export(..) | Declaration::Import(..)
			),
		}
	}
}

impl ASTNode for StatementOrDeclaration {
	fn get_position(&self) -> Cow<Span> {
		match self {
			StatementOrDeclaration::Statement(item) => item.get_position(),
			StatementOrDeclaration::Declaration(item) => item.get_position(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		if Declaration::is_declaration_start(reader) {
			let dec = Declaration::from_reader(reader, state, settings)?;
			// TODO nested blocks? Interfaces...?
			Ok(StatementOrDeclaration::Declaration(dec))
		} else {
			let stmt = Statement::from_reader(reader, state, settings)?;
			Ok(StatementOrDeclaration::Statement(stmt))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		match self {
			StatementOrDeclaration::Statement(item) => {
				item.to_string_from_buffer(buf, settings, depth)
			}
			StatementOrDeclaration::Declaration(item) => {
				item.to_string_from_buffer(buf, settings, depth)
			}
		}
	}
}

/// A "block" of braced statements and declarations
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct Block(pub Vec<StatementOrDeclaration>, pub BlockId, pub Span);

impl Eq for Block {}

impl PartialEq for Block {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}

pub struct BlockLike<'a> {
	pub block_id: BlockId,
	pub items: &'a Vec<StatementOrDeclaration>,
}

pub struct BlockLikeMut<'a> {
	pub block_id: BlockId,
	pub items: &'a mut Vec<StatementOrDeclaration>,
}

impl<'a> From<&'a Block> for BlockLike<'a> {
	fn from(block: &'a Block) -> Self {
		BlockLike { block_id: block.1, items: &block.0 }
	}
}

impl<'a> From<&'a mut Block> for BlockLikeMut<'a> {
	fn from(block: &'a mut Block) -> Self {
		BlockLikeMut { block_id: block.1, items: &mut block.0 }
	}
}

impl ASTNode for Block {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start_span = reader.expect_next(TSXToken::OpenBrace)?;
		let (items, block_id) = parse_statements_and_declarations(reader, state, settings)?;
		let end_span = reader.expect_next(TSXToken::CloseBrace)?;
		Ok(Self(items, block_id, start_span.union(&end_span)))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push('{');
		if depth > 0 && settings.pretty {
			buf.push_new_line();
		}
		statements_and_declarations_to_string(&self.0, buf, settings, depth);
		if settings.pretty {
			buf.push_new_line();
		}
		if depth > 1 {
			settings.add_indent(depth - 1, buf);
		}
		buf.push('}');
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.2)
	}
}

impl Block {
	pub fn iter(&self) -> core::slice::Iter<'_, StatementOrDeclaration> {
		self.0.iter()
	}

	pub fn iter_mut(&mut self) -> core::slice::IterMut<'_, StatementOrDeclaration> {
		self.0.iter_mut()
	}
}

impl Visitable for Block {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,

		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		{
			visitors.visit_block(
				&crate::block::BlockLike { block_id: self.1, items: &self.0 },
				data,
				chain,
			);
		}
		let iter = self.iter();
		if settings.reverse_statements {
			iter.rev().for_each(|item| item.visit(visitors, data, settings, chain));
		} else {
			iter.for_each(|item| item.visit(visitors, data, settings, chain));
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,

		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut { block_id: self.1, items: &mut self.0 },
				data,
				chain,
			);
		}
		let iter_mut = self.iter_mut();
		if settings.reverse_statements {
			iter_mut.for_each(|statement| statement.visit_mut(visitors, data, settings, chain));
		} else {
			iter_mut
				.rev()
				.for_each(|statement| statement.visit_mut(visitors, data, settings, chain));
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
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		match self {
			BlockOrSingleStatement::Braced(block) => {
				block.to_string_from_buffer(buf, settings, depth)
			}
			BlockOrSingleStatement::SingleStatement(stmt) => {
				if settings.pretty {
					buf.push_new_line();
					settings.add_gap(buf);
					stmt.to_string_from_buffer(buf, settings, depth);
				} else {
					stmt.to_string_from_buffer(buf, settings, depth);
				}
			}
		}
	}
}

/// Parse statements, regardless of bracing or not
pub(crate) fn parse_statements_and_declarations(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseSettings,
) -> ParseResult<(Vec<StatementOrDeclaration>, BlockId)> {
	let mut items = Vec::new();
	let block_id = BlockId::new();
	while let Some(Token(token_type, _)) = reader.peek() {
		if let TSXToken::EOS | TSXToken::CloseBrace = token_type {
			break;
		}

		let value = StatementOrDeclaration::from_reader(reader, state, settings)?;
		if value.requires_semi_colon() {
			expect_semi_colon(reader)?;
		}
		items.push(value);
	}
	Ok((items, block_id))
}

pub fn statements_and_declarations_to_string<T: source_map::ToString>(
	items: &[StatementOrDeclaration],
	buf: &mut T,
	settings: &crate::ToStringSettings,
	depth: u8,
) {
	for (not_at_end, item) in items.iter().nendiate() {
		settings.add_indent(depth, buf);
		item.to_string_from_buffer(buf, settings, depth);
		if not_at_end {
			// TODO only append new line if something added
			if item.requires_semi_colon() {
				buf.push(';');
			}
			if settings.pretty {
				buf.push_new_line();
			}
		}
	}
}

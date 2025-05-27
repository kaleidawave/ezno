use derive_enum_from_into::EnumFrom;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	derive_ASTNode, ASTNode, ParseResult, Span, Statement, StatementOrDeclaration, VisitOptions,
	Visitable,
};

/// A "block" of braced statements and declarations
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Block(pub Vec<StatementOrDeclaration>, pub Span);

impl Eq for Block {}

impl PartialEq for Block {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}

pub struct BlockLike<'a> {
	pub items: &'a Vec<StatementOrDeclaration>,
}

pub struct BlockLikeMut<'a> {
	pub items: &'a mut Vec<StatementOrDeclaration>,
}

impl<'a> From<&'a Block> for BlockLike<'a> {
	fn from(block: &'a Block) -> Self {
		BlockLike { items: &block.0 }
	}
}

impl<'a> From<&'a mut Block> for BlockLikeMut<'a> {
	fn from(block: &'a mut Block) -> Self {
		BlockLikeMut { items: &mut block.0 }
	}
}

impl ASTNode for Block {
	fn get_position(&self) -> Span {
		self.1
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_start('{')?;
		let items = statements_and_declarations_from_reader(reader)?;
		let position = start.union(reader.expect('}')?);
		Ok(Block(items, position))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.pretty && self.0.is_empty() {
			buf.push_str("{}");
		} else {
			buf.push('{');
			if local.depth > 0 && options.pretty {
				buf.push_new_line();
			}
			statements_and_declarations_to_string(&self.0, buf, options, local);
			if options.pretty && !self.0.is_empty() {
				buf.push_new_line();
			}
			if local.depth > 1 {
				options.add_indent(local.depth - 1, buf);
			}
			buf.push('}');
		}
	}
}

impl Block {
	pub fn items(&self) -> core::slice::Iter<'_, StatementOrDeclaration> {
		self.0.iter()
	}

	pub fn items_mut(&mut self) -> core::slice::IterMut<'_, StatementOrDeclaration> {
		self.0.iter_mut()
	}
}

/// For ifs and other statements bodies
#[derive(Debug, Clone, PartialEq, EnumFrom)]
#[apply(derive_ASTNode!)]
pub enum BlockOrSingleStatement {
	Braced(Block),
	SingleStatement(Box<Statement>),
}

impl ASTNode for BlockOrSingleStatement {
	fn get_position(&self) -> Span {
		match self {
			BlockOrSingleStatement::Braced(blk) => blk.get_position(),
			BlockOrSingleStatement::SingleStatement(stmt) => stmt.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let stmt = Statement::from_reader(reader)?;
		if let StatementOrDeclaration::Block(blk) = stmt.0 {
			Ok(Self::Braced(blk))
		} else {
			if stmt.0.requires_semi_colon() {
				reader.expect_semi_colon()?;
			}
			Ok(Self::SingleStatement(Box::new(stmt)))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if buf.should_halt() {
			return;
		}
		match self {
			BlockOrSingleStatement::Braced(block) => {
				block.to_string_from_buffer(buf, options, local);
			}
			BlockOrSingleStatement::SingleStatement(statement) => {
				if let StatementOrDeclaration::Empty(..) = statement.0 {
					buf.push(';');
				} else if options.pretty && !options.single_statement_on_new_line {
					buf.push_new_line();
					options.push_gap_optionally(buf);
					statement.to_string_from_buffer(buf, options, local.next_level());
				} else {
					statement.to_string_from_buffer(buf, options, local);
					if statement.0.requires_semi_colon() {
						buf.push(';');
					}
				}
			}
		}
	}
}

/// Parse statements, regardless of bracing or not
pub(crate) fn statements_and_declarations_from_reader(
	reader: &mut crate::Lexer,
) -> ParseResult<Vec<StatementOrDeclaration>> {
	let mut items = Vec::new();
	loop {
		reader.skip();

		let at_end = reader.is_finished() || reader.starts_with('}');

		if at_end {
			break;
		}

		if reader.get_options().retain_blank_lines && !items.is_empty() {
			let new_lines = reader.last_was_from_new_line();
			for _ in 0..new_lines {
				// TODO span
				let start = reader.get_start().0;
				let end = reader.get_end().0;
				let span = Span { start, end, source: () };
				items.push(StatementOrDeclaration::Empty(span));
			}
		}

		let item = StatementOrDeclaration::from_reader(reader)?;

		// Skip emptyies at the start
		if let (true, StatementOrDeclaration::Empty(..)) = (items.is_empty(), &item) {
			continue;
		}

		if item.requires_semi_colon() {
			reader.expect_semi_colon()?;
		}

		items.push(item);
	}
	Ok(items)
}

pub fn statements_and_declarations_to_string<T: source_map::ToString>(
	items: &[StatementOrDeclaration],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	// let mut last_was_empty = false;
	for (at_end, item) in items.iter().endiate() {
		if !options.pretty {
			if let StatementOrDeclaration::Expression(crate::expressions::MultipleExpression(
				crate::Expression::Null(..),
			)) = item
			{
				continue;
			}
		}

		// if options.pretty {
		// 	// Don't print more than two lines in a row
		// 	if let StatementOrDeclaration::AestheticSemiColon(_)
		// 	| StatementOrDeclaration::Empty(_) = item
		// 	{
		// 		if last_was_empty {
		// 			continue;
		// 		}
		// 		last_was_empty = true;
		// 	} else {
		// 		last_was_empty = false;
		// 	}
		// }

		if !options.include_type_annotations {
			match item {
				StatementOrDeclaration::Function(item) if item.on.name.is_declare => {
					continue;
				}
				StatementOrDeclaration::Class(item) if item.on.name.is_declare => {
					continue;
				}
				_ => {}
			}
		}

		options.add_indent(local.depth, buf);
		item.to_string_from_buffer(buf, options, local);
		if (!at_end || options.trailing_semicolon) && item.requires_semi_colon() {
			buf.push(';');
		}
		if !at_end && options.pretty {
			buf.push_new_line();
		}
	}
}

impl Visitable for Block {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		if options.visit_nested_blocks || chain.is_empty() {
			{
				visitors.visit_block(&crate::block::BlockLike { items: &self.0 }, data, chain);
			}
			let items = self.items();
			if options.reverse_statements {
				items.rev().for_each(|item| item.visit(visitors, data, options, chain));
			} else {
				items.for_each(|item| item.visit(visitors, data, options, chain));
			}
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		if options.visit_nested_blocks || chain.is_empty() {
			{
				visitors.visit_block_mut(
					&mut crate::block::BlockLikeMut { items: &mut self.0 },
					data,
					chain,
				);
			}
			let items = self.items_mut();
			if options.reverse_statements {
				items.for_each(|statement| statement.visit_mut(visitors, data, options, chain));
			} else {
				items
					.rev()
					.for_each(|statement| statement.visit_mut(visitors, data, options, chain));
			}
		}
	}
}

impl Visitable for BlockOrSingleStatement {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::visiting::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			BlockOrSingleStatement::Braced(b) => {
				b.visit(visitors, data, options, chain);
			}
			BlockOrSingleStatement::SingleStatement(s) => {
				s.visit(visitors, data, options, chain);
				visitors.visit_statement(&s.0, data, chain);
			}
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::visiting::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			BlockOrSingleStatement::Braced(ref mut b) => {
				b.visit_mut(visitors, data, options, chain);
			}
			BlockOrSingleStatement::SingleStatement(ref mut s) => {
				s.visit_mut(visitors, data, options, chain);
				visitors.visit_statement_mut(&mut s.0, data, chain);
			}
		}
	}
}

impl From<Statement> for BlockOrSingleStatement {
	fn from(stmt: Statement) -> Self {
		Self::SingleStatement(Box::new(stmt))
	}
}

impl From<crate::Expression> for StatementOrDeclaration {
	fn from(expr: crate::Expression) -> Self {
		StatementOrDeclaration::Expression(crate::expressions::MultipleExpression::from(expr))
	}
}

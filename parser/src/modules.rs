use crate::{
	block::{parse_statements_and_declarations, statements_and_declarations_to_string},
	derive_ASTNode, BlockLike, BlockLikeMut, LocalToStringInformation, ParseOptions, ParseResult,
	StatementOrDeclaration, VisitOptions,
};

use super::{ASTNode, Span, TSXToken, TokenReader};

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct Module {
	pub hashbang_comment: Option<String>,
	pub items: Vec<StatementOrDeclaration>,
	pub span: Span,
}

impl PartialEq for Module {
	fn eq(&self, other: &Self) -> bool {
		self.items == other.items
	}
}

impl ASTNode for Module {
	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(ref hashbang_comment) = self.hashbang_comment {
			buf.push_str("#!");
			buf.push_str(hashbang_comment);
			buf.push_new_line();
		}
		statements_and_declarations_to_string(&self.items, buf, options, local);
	}

	fn get_position(&self) -> Span {
		self.span
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"let span = Span { start: 0, source: (), end: state.length_of_source };
		let hashbang_comment = if let Some(crate::Token(TSXToken::HashBangComment(_), _)) =
			reader.peek()
		{
			let Some(crate::Token(TSXToken::HashBangComment(hashbang_comment), _)) = reader.next()
			else {
				unreachable!()
			};
			Some(hashbang_comment)
		} else {
			None
		};
		parse_statements_and_declarations(reader, state, options).map(|items| Module {
			hashbang_comment,
			items,
			span,
		})"#;
		todo!();
	}
}

impl Module {
	pub fn to_string_with_source_map(
		&self,
		options: &crate::ToStringOptions,
		this: source_map::SourceId,
		fs: &impl source_map::FileSystem,
	) -> (String, Option<source_map::SourceMap>) {
		let mut buf = source_map::StringWithOptionalSourceMap::new(true);
		self.to_string_from_buffer(
			&mut buf,
			options,
			LocalToStringInformation { depth: 0, under: this, should_try_pretty_print: true },
		);
		buf.build(fs)
	}

	// #[must_use]
	// pub fn length(&self, options: &crate::ToStringOptions) -> usize {
	// 	let mut buf = source_map::Counter::new();
	// 	self.to_string_from_buffer(
	// 		&mut buf,
	// 		options,
	// 		LocalToStringInformation { depth: 0, under: source_map::Nullable::NULL },
	// 	);
	// 	buf.get_count()
	// }
}

impl Module {
	pub fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		source: source_map::SourceId,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block(&crate::block::BlockLike { items: &self.items }, data, &chain);
		}

		let iter = self.items.iter();
		if options.reverse_statements {
			iter.for_each(|item| item.visit(visitors, data, options, &mut chain));
		} else {
			iter.rev().for_each(|item| item.visit(visitors, data, options, &mut chain));
		}
	}

	pub fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		source: source_map::SourceId,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut { items: &mut self.items },
				data,
				&chain,
			);
		}

		let iter_mut = self.items.iter_mut();
		if options.reverse_statements {
			iter_mut.for_each(|item| item.visit_mut(visitors, data, options, &mut chain));
		} else {
			iter_mut.rev().for_each(|item| item.visit_mut(visitors, data, options, &mut chain));
		}
	}
}

impl<'a> From<&'a Module> for BlockLike<'a> {
	fn from(module: &'a Module) -> Self {
		BlockLike { items: &module.items }
	}
}

impl<'a> From<&'a mut Module> for BlockLikeMut<'a> {
	fn from(module: &'a mut Module) -> Self {
		BlockLikeMut { items: &mut module.items }
	}
}

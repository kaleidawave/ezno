use std::borrow::Cow;

use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};
use visitable_derive::Visitable;

use crate::{
	extractor::ExtractedFunctions, tokens::token_as_identifier, ASTNode, Expression, ParseResult,
	ParseSettings, TSXToken, Visitable,
};

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct Decorator {
	pub name: String,
	pub arguments: Option<Vec<Expression>>,
	pub position: Span,
}

impl ASTNode for Decorator {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let at_pos = reader.expect_next(TSXToken::At)?;
		Self::from_reader_sub_at_symbol(reader, state, settings, at_pos)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if settings.0.include_decorators {
			buf.push('@');
			buf.push_str(self.name.as_str());
			if let Some(arguments) = &self.arguments {
				buf.push('(');
				for (at_end, argument) in arguments.iter().endiate() {
					argument.to_string_from_buffer(buf, settings, depth);
					if !at_end {
						buf.push(',');
						settings.0.add_gap(buf);
					}
				}
				buf.push(')');
			}
		}
	}
}

impl Decorator {
	pub(crate) fn from_reader_sub_at_symbol(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		at_pos: Span,
	) -> ParseResult<Self> {
		let (name, name_position) = token_as_identifier(reader.next().unwrap(), "Decorator name")?;
		let (arguments, position) = if matches!(reader.peek().unwrap().0, TSXToken::OpenParentheses)
		{
			reader.next();
			let mut arguments = Vec::<_>::new();
			loop {
				if matches!(reader.peek().unwrap().0, TSXToken::CloseParentheses) {
					break;
				}
				arguments.push(Expression::from_reader(reader, state, settings)?);
				match reader.peek() {
					Some(Token(TSXToken::Comma, _)) => {
						reader.next();
					}
					_ => break,
				}
			}
			let end_position = reader.expect_next(TSXToken::CloseParentheses)?;
			(Some(arguments), at_pos.union(&end_position))
		} else {
			(None, at_pos.union(&name_position))
		};
		Ok(Self { name, arguments, position })
	}
}

/// TODO under cfg if don't want this could just be `type Decorated<T> = T;`
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct Decorated<T> {
	pub decorators: Vec<Decorator>,
	pub on: T,
}

impl<N: ASTNode> ASTNode for Decorated<N> {
	fn get_position(&self) -> Cow<Span> {
		// TODO union with first decorated
		self.on.get_position()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader, state, settings)?;
		N::from_reader(reader, state, settings).map(|on| Self { on, decorators })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		self.to_string_from_buffer_just_decorators(buf, settings, depth);
		self.on.to_string_from_buffer(buf, settings, depth);
	}
}

impl<U> Decorated<U> {
	pub fn new(on: U) -> Self {
		Self { decorators: Default::default(), on }
	}

	pub(crate) fn to_string_from_buffer_just_decorators<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if settings.0.include_decorators {
			for decorator in self.decorators.iter() {
				decorator.to_string_from_buffer(buf, settings, depth);
				if settings.0.pretty {
					buf.push_new_line();
				} else {
					buf.push(' ');
				}
			}
		}
	}
}

pub(crate) fn decorators_from_reader(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseSettings,
) -> ParseResult<Vec<Decorator>> {
	let mut decorators = Vec::new();
	while let Some(Token(TSXToken::At, _)) = reader.peek() {
		decorators.push(Decorator::from_reader(reader, state, settings)?);
	}
	Ok(decorators)
}

impl<T: Visitable> Visitable for Decorated<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit(visitors, data, settings, functions, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit_mut(visitors, data, settings, functions, chain);
	}
}

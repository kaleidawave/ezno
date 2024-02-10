use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{
	sized_tokens::{TokenReaderWithTokenEnds, TokenStart},
	Token, TokenReader,
};
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode, tokens::token_as_identifier, ASTNode, Expression, ParseOptions, ParseResult,
	TSXToken, Visitable,
};

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub struct Decorator {
	pub name: Vec<String>,
	pub arguments: Option<Vec<Expression>>,
	pub position: Span,
}

impl ASTNode for Decorator {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let at_pos = reader.expect_next(TSXToken::At)?;
		Self::from_reader_sub_at_symbol(reader, state, options, at_pos)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_decorators {
			buf.push('@');
			for (not_at_end, value) in self.name.iter().nendiate() {
				buf.push_str(value);
				if not_at_end {
					buf.push('.');
				}
			}
			if let Some(arguments) = &self.arguments {
				buf.push('(');
				for (at_end, argument) in arguments.iter().endiate() {
					argument.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				buf.push(')');
			}
		}
	}
}

impl Decorator {
	pub(crate) fn from_reader_sub_at_symbol(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		at_pos: TokenStart,
	) -> ParseResult<Self> {
		let (name, mut last_position) =
			token_as_identifier(reader.next().unwrap(), "Decorator name")?;

		let mut names = vec![name];
		while matches!(reader.peek(), Some(Token(TSXToken::Dot, _))) {
			let (name, pos) = token_as_identifier(reader.next().unwrap(), "Nested decorator name")?;
			last_position = pos;
			names.push(name);
		}

		let (arguments, position) = if reader
			.conditional_next(|token| matches!(token, TSXToken::OpenParentheses))
			.is_some()
		{
			let mut arguments = Vec::<_>::new();
			loop {
				if let Some(Token(TSXToken::CloseParentheses, _)) = reader.peek() {
					break;
				}
				arguments.push(Expression::from_reader(reader, state, options)?);
				match reader.peek() {
					Some(Token(TSXToken::Comma, _)) => {
						reader.next();
					}
					_ => break,
				}
			}
			let end = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
			(Some(arguments), at_pos.union(end))
		} else {
			(None, at_pos.union(last_position))
		};
		Ok(Self { name: names, arguments, position })
	}
}

/// TODO under cfg if don't want this could just be `type Decorated<T> = T;`
#[derive(Debug, PartialEq, Eq, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct Decorated<T> {
	pub decorators: Vec<Decorator>,
	pub on: T,
	// TODO option and on t
	pub position: Span,
}

impl<N: ASTNode> ASTNode for Decorated<N> {
	fn get_position(&self) -> &Span {
		self.get()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader, state, options)?;
		N::from_reader(reader, state, options).map(|on| Self::new(decorators, on))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.to_string_from_buffer_just_decorators(buf, options, local);
		self.on.to_string_from_buffer(buf, options, local);
	}
}

impl<U: ASTNode> Decorated<U> {
	pub fn new_empty(on: U) -> Self {
		Self::new(Default::default(), on)
	}

	pub fn new(decorators: Vec<Decorator>, on: U) -> Self {
		let position =
			decorators.first().map_or(on.get_position(), |d| &d.position).union(on.get_position());
		Self { decorators, on, position }
	}

	pub(crate) fn to_string_from_buffer_just_decorators<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_decorators {
			for decorator in &self.decorators {
				decorator.to_string_from_buffer(buf, options, local);
				if options.pretty {
					buf.push_new_line();
				} else {
					buf.push(' ');
				}
			}
		}
	}
}

pub(crate) fn decorators_from_reader(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
) -> ParseResult<Vec<Decorator>> {
	let mut decorators = Vec::new();
	while let Some(Token(TSXToken::At, _)) = reader.peek() {
		decorators.push(Decorator::from_reader(reader, state, options)?);
	}
	Ok(decorators)
}

impl<T: Visitable> Visitable for Decorated<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,

		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit(visitors, data, options, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,

		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit_mut(visitors, data, options, chain);
	}
}

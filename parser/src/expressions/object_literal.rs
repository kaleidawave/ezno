use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use std::{fmt::Debug, mem};
use tokenizer_lib::sized_tokens::{TokenReaderWithTokenEnds, TokenStart};
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, functions::FunctionBased, property_key::AlwaysPublic,
	throw_unexpected_token_with_token, ASTNode, Block, Expression, FunctionBase,
	GetSetGeneratorOrNone, Keyword, ParseOptions, ParseResult, PropertyKey, Span, TSXToken, Token,
	TokenReader, WithComment,
};

#[derive(Debug, Clone, Eq, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct ObjectLiteral {
	pub members: Vec<ObjectLiteralMember>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEqExtras)]
#[partial_eq_ignore_types(Span, VariableId)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ObjectLiteralMember {
	SpreadExpression(Expression, Span),
	Shorthand(String, Span),
	Property(WithComment<PropertyKey<AlwaysPublic>>, Expression, Span),
	Method(ObjectLiteralMethod),
}

impl crate::Visitable for ObjectLiteralMember {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::SpreadExpression(_, _) => {}
			ObjectLiteralMember::Shorthand(_, _) => {}
			ObjectLiteralMember::Property(_, _, _) => {}
			ObjectLiteralMember::Method(method) => method.visit(visitors, data, settings, chain),
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::SpreadExpression(_, _) => {}
			ObjectLiteralMember::Shorthand(_, _) => {}
			ObjectLiteralMember::Property(_, _, _) => {}
			ObjectLiteralMember::Method(method) => {
				method.visit_mut(visitors, data, settings, chain)
			}
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ObjectLiteralMethodBase;
pub type ObjectLiteralMethod = FunctionBase<ObjectLiteralMethodBase>;

impl FunctionBased for ObjectLiteralMethodBase {
	type Name = WithComment<PropertyKey<AlwaysPublic>>;
	type Header = (Option<Keyword<crate::tsx_keywords::Async>>, GetSetGeneratorOrNone);
	type Body = Block;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassMethod(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let async_keyword = crate::Keyword::optionally_from_reader(reader);
		Ok((
			(async_keyword, GetSetGeneratorOrNone::from_reader(reader)),
			WithComment::<PropertyKey<_>>::from_reader(reader, state, settings)?,
		))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if header.0.is_some() {
			buf.push_str("async ");
		}
		header.1.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, settings, depth);
	}

	fn header_left(header: &Self::Header) -> Option<&Span> {
		if let Some(ref async_kw) = header.0 {
			Some(&async_kw.1)
		} else {
			header.1.get_position()
		}
	}
}

impl Eq for ObjectLiteralMember {}

impl ASTNode for ObjectLiteral {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::OpenBrace)?;
		Self::from_reader_sub_open_curly(reader, state, settings, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push('{');
		settings.add_gap(buf);
		for (at_end, member) in self.members.iter().endiate() {
			member.to_string_from_buffer(buf, settings, depth + 1);
			if !at_end {
				buf.push(',');
				settings.add_gap(buf);
			}
		}
		settings.add_gap(buf);
		buf.push('}');
	}
}

impl ObjectLiteral {
	pub(crate) fn from_reader_sub_open_curly(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut members: Vec<ObjectLiteralMember> = Vec::new();
		loop {
			if matches!(reader.peek(), Some(Token(TSXToken::CloseBrace, _))) {
				break;
			}
			members.push(ObjectLiteralMember::from_reader(reader, state, settings)?);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		let end = reader.expect_next_get_end(TSXToken::CloseBrace)?;
		Ok(ObjectLiteral { members, position: start.union(end) })
	}
}

impl ASTNode for ObjectLiteralMember {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let async_kw = Keyword::optionally_from_reader(reader);
		// TODO this probably needs with comment here:
		let mut get_set_generator_or_none = GetSetGeneratorOrNone::from_reader(reader);
		// Catch for named get or set :(
		let is_named_get_or_set = matches!(
			(reader.peek(), &get_set_generator_or_none),
			(
				Some(Token(TSXToken::OpenParentheses | TSXToken::Colon, _)),
				GetSetGeneratorOrNone::Get(..) | GetSetGeneratorOrNone::Set(..)
			)
		);

		let key = if is_named_get_or_set {
			// Backtrack allowing `get` to be a key
			let (name, position) = match mem::take(&mut get_set_generator_or_none) {
				GetSetGeneratorOrNone::Get(kw) => ("get", kw.1),
				GetSetGeneratorOrNone::Set(kw) => ("set", kw.1),
				_ => unreachable!(),
			};
			WithComment::None(PropertyKey::Ident(name.to_owned(), position, ()))
		} else {
			WithComment::<PropertyKey<_>>::from_reader(reader, state, settings)?
		};
		let Token(token, _) = &reader.peek().ok_or_else(parse_lexing_error)?;
		match token {
			// Functions, (OpenChevron is for generic parameters)
			TSXToken::OpenParentheses | TSXToken::OpenChevron => {
				let method: ObjectLiteralMethod = FunctionBase::from_reader_with_header_and_name(
					reader,
					state,
					settings,
					(async_kw, get_set_generator_or_none),
					key,
				)?;

				Ok(Self::Method(method))
			}
			_ => {
				if get_set_generator_or_none != GetSetGeneratorOrNone::None {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				if matches!(reader.peek(), Some(Token(TSXToken::Comma | TSXToken::CloseBrace, _))) {
					// TODO fix
					if let PropertyKey::Ident(name, position, _) = key.get_ast() {
						Ok(Self::Shorthand(name, position))
					} else {
						let token = reader.next().ok_or_else(parse_lexing_error)?;
						throw_unexpected_token_with_token(token, &[TSXToken::Colon])
					}
				} else {
					reader.expect_next(TSXToken::Colon)?;
					let expression = Expression::from_reader(reader, state, settings)?;
					let position = key.get_position().union(expression.get_position());
					Ok(Self::Property(key, expression, position))
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Property(name, expression, _) => {
				name.to_string_from_buffer(buf, settings, depth);
				buf.push(':');
				settings.add_gap(buf);
				expression.to_string_from_buffer(buf, settings, depth);
			}
			Self::Shorthand(name, ..) => {
				buf.push_str(name.as_str());
			}
			Self::Method(func) => {
				func.to_string_from_buffer(buf, settings, depth);
			}
			Self::SpreadExpression(spread_expr, _) => {
				buf.push_str("...");
				spread_expr.to_string_from_buffer(buf, settings, depth);
			}
		};
	}

	fn get_position(&self) -> &Span {
		match self {
			Self::Method(method) => method.get_position(),
			Self::Shorthand(_, pos)
			| Self::Property(_, _, pos)
			| Self::SpreadExpression(_, pos) => pos,
		}
	}
}

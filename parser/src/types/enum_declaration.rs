use crate::{TSXKeyword, TSXToken};
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenReaderWithTokenEnds, Token};
use visitable_derive::Visitable;

use crate::{errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression};

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct EnumDeclaration {
	pub is_constant: bool,
	pub name: String,
	pub members: Vec<EnumMember>,
	pub position: Span,
}

impl ASTNode for EnumDeclaration {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> Result<Self, crate::ParseError> {
		let const_pos = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Const)))
			.map(|Token(_, pos)| pos);

		let is_constant = const_pos.is_some();
		let enum_pos = state.new_keyword(reader, TSXKeyword::Enum)?;
		let (name, _) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "Enum name")?;
		reader.expect_next(TSXToken::OpenBrace)?;
		let mut members = Vec::new();
		loop {
			if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
				break;
			}
			members.push(EnumMember::from_reader(reader, state, options)?);
			// Commas are optional
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			}
		}
		let end = reader.expect_next_get_end(TSXToken::CloseBrace)?;
		Ok(EnumDeclaration {
			is_constant,
			position: const_pos.unwrap_or(enum_pos).union(end),
			name,
			members,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		if self.is_constant {
			buf.push_str("const ");
		}
		buf.push_str("enum ");
		buf.push_str(&self.name);
		options.add_gap(buf);
		buf.push_str("{");
		for (at_end, member) in self.members.iter().endiate() {
			if options.pretty {
				buf.push_new_line();
				options.add_indent(depth + 1, buf);
			}
			member.to_string_from_buffer(buf, options, depth);
			if !options.pretty && !at_end {
				buf.push(',');
			}
		}
		if options.pretty && !self.members.is_empty() {
			buf.push_new_line();
		}
		buf.push('}');
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum EnumMember {
	Variant { name: String, value: Option<Expression>, position: Span },
}

impl ASTNode for EnumMember {
	fn get_position(&self) -> &Span {
		match self {
			EnumMember::Variant { position, .. } => position,
		}
	}

	fn from_reader(
		reader: &mut impl tokenizer_lib::TokenReader<crate::TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> Result<Self, crate::ParseError> {
		let (name, start_pos) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "Enum variant")?;
		match reader.peek() {
			Some(Token(TSXToken::Assign, _)) => {
				reader.next();
				let expression = Expression::from_reader(reader, state, options)?;
				Ok(EnumMember::Variant {
					name,
					position: start_pos.union(expression.get_position()),
					value: Some(expression),
				})
			}
			_ => Ok(EnumMember::Variant { name, value: None, position: start_pos }),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			EnumMember::Variant { name, value, .. } => {
				buf.push_str(name);
				if let Some(value) = value {
					buf.push_str(if options.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, options, depth);
				}
			}
		}
	}
}

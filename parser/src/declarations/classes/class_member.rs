use std::fmt::Debug;

use crate::{errors::parse_lexing_error, property_key::PublicOrPrivate, tsx_keywords};
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};
use visitable_derive::Visitable;

use crate::{
	functions::FunctionBased, ASTNode, Block, Expression, FunctionBase, Keyword, MethodHeader,
	ParseOptions, ParseResult, PropertyKey, TSXKeyword, TSXToken, TypeAnnotation, WithComment,
};

/// The variable id's of these is handled by their [PropertyKey]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ClassMember {
	Constructor(ClassConstructor),
	Method(Option<Keyword<tsx_keywords::Static>>, ClassFunction),
	Property(Option<Keyword<tsx_keywords::Static>>, ClassProperty),
	StaticBlock(Block),
	Comment(String, Span),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassConstructorBase;
pub type ClassConstructor = FunctionBase<ClassConstructorBase>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassFunctionBase;
pub type ClassFunction = FunctionBase<ClassFunctionBase>;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ClassProperty {
	pub readonly_keyword: Option<Keyword<tsx_keywords::Readonly>>,
	pub key: WithComment<PropertyKey<PublicOrPrivate>>,
	pub type_annotation: Option<TypeAnnotation>,
	pub value: Option<Box<Expression>>,
	pub position: Span,
}

impl ASTNode for ClassMember {
	fn get_position(&self) -> &Span {
		match self {
			Self::Constructor(cst) => cst.get_position(),
			Self::Method(_, mtd) => mtd.get_position(),
			Self::Property(_, prop) => &prop.position,
			Self::StaticBlock(blk) => blk.get_position(),
			Self::Comment(_, pos) => pos,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::MultiLineComment(_), _)) = reader.peek() {
			let Some(Token(TSXToken::MultiLineComment(c), start)) = reader.next() else {
				unreachable!()
			};
			let with_length = start.with_length(c.len() + 2);
			return Ok(Self::Comment(c, with_length));
		}

		if let Some(Token(TSXToken::Keyword(TSXKeyword::Constructor), _)) = reader.peek() {
			let constructor = ClassConstructor::from_reader(reader, state, settings)?;
			return Ok(ClassMember::Constructor(constructor));
		}

		let is_static = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Static))
			.map(|token| Keyword::new(token.get_span()));

		if let Some(Token(TSXToken::OpenBrace, _)) = reader.peek() {
			return Ok(ClassMember::StaticBlock(Block::from_reader(reader, state, settings)?));
		}

		let readonly_keyword = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Readonly))
			.map(|token| Keyword::new(token.get_span()));

		let header = MethodHeader::optional_from_reader(reader);
		let key = WithComment::<PropertyKey<_>>::from_reader(reader, state, settings)?;

		match reader.peek() {
			Some(Token(TSXToken::OpenParentheses, _)) if readonly_keyword.is_none() => {
				let function =
					ClassFunction::from_reader_with_config(reader, state, settings, header, key)?;
				Ok(ClassMember::Method(is_static, function))
			}
			Some(Token(token, _)) => {
				if header.is_some() {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				let member_type: Option<TypeAnnotation> = match token {
					TSXToken::Colon => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						Some(type_annotation)
					}
					_ => None,
				};
				let member_expression: Option<Expression> = match reader.peek() {
					Some(Token(TSXToken::Assign, _)) => {
						reader.next();
						let expression = Expression::from_reader(reader, state, settings)?;
						Some(expression)
					}
					_ => None,
				};
				Ok(Self::Property(
					is_static,
					ClassProperty {
						readonly_keyword,
						position: key.get_position().clone(),
						key,
						type_annotation: member_type,
						value: member_expression.map(Box::new),
					},
				))
			}
			None => Err(parse_lexing_error()),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Property(
				is_static,
				ClassProperty { readonly_keyword, key, type_annotation, value, position: _ },
			) => {
				if is_static.is_some() {
					buf.push_str("static ");
				}
				if readonly_keyword.is_some() {
					buf.push_str("readonly ");
				}
				key.to_string_from_buffer(buf, settings, depth);
				if let (true, Some(type_annotation)) = (settings.include_types, type_annotation) {
					buf.push_str(": ");
					type_annotation.to_string_from_buffer(buf, settings, depth);
				}
				if let Some(value) = value {
					buf.push_str(if settings.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, settings, depth);
				}
			}
			Self::Method(is_static, function) => {
				if is_static.is_some() {
					buf.push_str("static ");
				}
				function.to_string_from_buffer(buf, settings, depth + 1)
			}
			Self::Constructor(constructor) => {
				constructor.to_string_from_buffer(buf, settings, depth + 1)
			}
			Self::StaticBlock(block) => {
				buf.push_str("static ");
				block.to_string_from_buffer(buf, settings, depth + 1);
			}
			Self::Comment(c, _) => {
				if settings.include_comments {
					buf.push_str("/*");
					buf.push_str(c);
					buf.push_str("*/")
				}
			}
		}
	}
}

impl ClassMember {
	// pub fn get_property_id(&self) -> Option<PropertyId> {
	// 	match self {
	// 		ClassMember::Method(_, ClassMethod { key, .. })
	// 		| ClassMember::Property(_, ClassProperty { key, .. }) => Some(key.get_ast().get_property_id()),
	// 		ClassMember::Constructor { .. } => None,
	// 	}
	// }
}

impl ClassFunction {
	fn from_reader_with_config(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		get_set_generator: Option<MethodHeader>,
		key: WithComment<PropertyKey<PublicOrPrivate>>,
	) -> ParseResult<Self> {
		FunctionBase::from_reader_with_header_and_name(
			reader,
			state,
			settings,
			get_set_generator,
			key,
		)
	}
}

impl FunctionBased for ClassFunctionBase {
	type Body = Block;
	type Header = Option<MethodHeader>;
	type Name = WithComment<PropertyKey<PublicOrPrivate>>;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassMethod(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let header = MethodHeader::optional_from_reader(reader);
		let name = WithComment::<PropertyKey<_>>::from_reader(reader, state, settings)?;
		Ok((header, name))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if let Some(header) = header {
			header.to_string_from_buffer(buf);
		}
		name.to_string_from_buffer(buf, settings, depth);
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		header.as_ref().map(|header| header.get_start())
	}
}

impl FunctionBased for ClassConstructorBase {
	type Body = Block;
	type Header = Keyword<tsx_keywords::Constructor>;
	type Name = ();

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassConstructor(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		Ok((Keyword::from_reader(reader)?, ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		_header: &Self::Header,
		_name: &Self::Name,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		buf.push_str("constructor")
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		Some(header.get_position().get_start())
	}
}

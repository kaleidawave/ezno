use std::fmt::Debug;

use crate::{
	errors::parse_lexing_error, property_key::PublicOrPrivate, tsx_keywords, visiting::Visitable,
};
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};
use visitable_derive::Visitable;

use crate::{
	functions::{FunctionBased, MethodHeader},
	ASTNode, Block, Expression, FunctionBase, Keyword, ParseOptions, ParseResult, PropertyKey,
	TSXKeyword, TSXToken, TypeAnnotation, WithComment,
};

/// The variable id's of these is handled by their [PropertyKey]
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
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
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if reader.peek().map_or(false, |t| t.0.is_comment()) {
			let Ok((comment, span)) = TSXToken::try_into_comment(reader.next().unwrap()) else {
				unreachable!()
			};
			return Ok(Self::Comment(comment, span));
		}

		if let Some(Token(TSXToken::Keyword(TSXKeyword::Constructor), _)) = reader.peek() {
			let constructor = ClassConstructor::from_reader(reader, state, options)?;
			return Ok(ClassMember::Constructor(constructor));
		}

		let is_static = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Static))
			.map(|token| Keyword::new(token.get_span()));

		if let Some(Token(TSXToken::OpenBrace, _)) = reader.peek() {
			return Ok(ClassMember::StaticBlock(Block::from_reader(reader, state, options)?));
		}

		let readonly_keyword = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Readonly))
			.map(|token| Keyword::new(token.get_span()));

		let header = MethodHeader::from_reader(reader);
		let key = WithComment::<PropertyKey<_>>::from_reader(reader, state, options)?;

		match reader.peek() {
			Some(Token(TSXToken::OpenParentheses, _)) if readonly_keyword.is_none() => {
				let function =
					ClassFunction::from_reader_with_config(reader, state, options, header, key)?;
				Ok(ClassMember::Method(is_static, function))
			}
			Some(Token(token, _)) => {
				if header.is_some() {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				let member_type: Option<TypeAnnotation> = match token {
					TSXToken::Colon => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
						Some(type_annotation)
					}
					_ => None,
				};
				let member_expression: Option<Expression> = match reader.peek() {
					Some(Token(TSXToken::Assign, _)) => {
						reader.next();
						let expression = Expression::from_reader(reader, state, options)?;
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
		options: &crate::ToStringOptions,
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
				key.to_string_from_buffer(buf, options, depth);
				if let (true, Some(type_annotation)) = (options.include_types, type_annotation) {
					buf.push_str(": ");
					type_annotation.to_string_from_buffer(buf, options, depth);
				}
				if let Some(value) = value {
					buf.push_str(if options.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, options, depth);
				}
			}
			Self::Method(is_static, function) => {
				if is_static.is_some() {
					buf.push_str("static ");
				}
				function.to_string_from_buffer(buf, options, depth + 1)
			}
			Self::Constructor(constructor) => {
				constructor.to_string_from_buffer(buf, options, depth + 1)
			}
			Self::StaticBlock(block) => {
				buf.push_str("static ");
				block.to_string_from_buffer(buf, options, depth + 1);
			}
			Self::Comment(c, _) => {
				if options.include_comments {
					buf.push_str("/*");
					buf.push_str(c);
					buf.push_str("*/")
				}
			}
		}
	}
}

impl ClassFunction {
	fn from_reader_with_config(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		get_set_generator: MethodHeader,
		key: WithComment<PropertyKey<PublicOrPrivate>>,
	) -> ParseResult<Self> {
		FunctionBase::from_reader_with_header_and_name(
			reader,
			state,
			options,
			get_set_generator,
			key,
		)
	}
}

impl FunctionBased for ClassFunctionBase {
	type Body = Block;
	type Header = MethodHeader;
	type Name = WithComment<PropertyKey<PublicOrPrivate>>;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let header = MethodHeader::from_reader(reader);
		let name = WithComment::<PropertyKey<_>>::from_reader(reader, state, options)?;
		Ok((header, name))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		header.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, options, depth);
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		header.get_start()
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit(visitors, data, options, chain)
	}

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit_mut(visitors, data, options, chain)
	}
}

impl FunctionBased for ClassConstructorBase {
	type Header = Keyword<tsx_keywords::Constructor>;
	type Name = ();
	type Body = Block;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassConstructor(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		Ok((Keyword::from_reader(reader)?, ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		_header: &Self::Header,
		_name: &Self::Name,
		_options: &crate::ToStringOptions,
		_depth: u8,
	) {
		buf.push_str("constructor")
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		Some(header.get_position().get_start())
	}

	fn visit_name<TData>(
		_: &Self::Name,
		_: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn visit_name_mut<TData>(
		_: &mut Self::Name,
		_: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}
}

use std::fmt::Debug;

use crate::{
	derive_ASTNode, errors::parse_lexing_error, functions::HeadingAndPosition,
	property_key::PublicOrPrivate, visiting::Visitable,
};
use source_map::Span;
use tokenizer_lib::{sized_tokens::TokenStart, Token, TokenReader};
use visitable_derive::Visitable;

use crate::{
	functions::{FunctionBased, MethodHeader},
	ASTNode, Block, Expression, FunctionBase, ParseOptions, ParseResult, PropertyKey, TSXKeyword,
	TSXToken, TypeAnnotation, WithComment,
};
#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type IsStatic = bool;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ClassMember {
	Constructor(ClassConstructor),
	Method(IsStatic, ClassFunction),
	Property(IsStatic, ClassProperty),
	StaticBlock(Block),
	Comment(String, bool, Span),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassConstructorBase;
pub type ClassConstructor = FunctionBase<ClassConstructorBase>;
#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
const CLASS_CONSTRUCTOR_TYPES: &str = r"
	export interface ClassConstructor extends Omit<FunctionBase, 'header' | 'name'> {
		body: Block
	}
";
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassFunctionBase;
pub type ClassFunction = FunctionBase<ClassFunctionBase>;
#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
const CLASS_FUNCTION_TYPES: &str = r"
	export interface ClassFunction extends FunctionBase {
		header: MethodHeader,
		body: Block,
		name: WithComment<PropertyKey<PublicOrPrivate>>
	}
";

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub struct ClassProperty {
	pub is_readonly: bool,
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
			Self::Comment(.., pos) => pos,
		}
	}

	#[allow(clippy::similar_names)]
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if reader.peek().map_or(false, |t| t.0.is_comment()) {
			let (comment, is_multiline, span) =
				TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
			return Ok(Self::Comment(comment, is_multiline, span));
		}

		if let Some(Token(TSXToken::Keyword(TSXKeyword::Constructor), _)) = reader.peek() {
			let constructor = ClassConstructor::from_reader(reader, state, options)?;
			return Ok(ClassMember::Constructor(constructor));
		}

		let is_static = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Static)))
			.is_some();

		if let Some(Token(TSXToken::OpenBrace, _)) = reader.peek() {
			return Ok(ClassMember::StaticBlock(Block::from_reader(reader, state, options)?));
		}

		let readonly_position = state.optionally_expect_keyword(reader, TSXKeyword::Readonly);

		// TODO not great
		let start = reader.peek().unwrap().1;

		let (header, key) = crate::functions::get_method_name(reader, state, options)?;

		match reader.peek() {
			Some(Token(TSXToken::OpenParentheses, _)) if readonly_position.is_none() => {
				let function = ClassFunction::from_reader_with_config(
					reader,
					state,
					options,
					(Some(start), header),
					key,
				)?;
				Ok(ClassMember::Method(is_static, function))
			}
			Some(Token(token, _)) => {
				if !header.is_no_modifiers() {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				let member_type: Option<TypeAnnotation> = if let TSXToken::Colon = token {
					reader.next();
					let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
					Some(type_annotation)
				} else {
					None
				};
				let member_expression: Option<Expression> =
					if let Some(Token(TSXToken::Assign, _)) = reader.peek() {
						reader.next();
						let expression = Expression::from_reader(reader, state, options)?;
						Some(expression)
					} else {
						None
					};
				Ok(Self::Property(
					is_static,
					ClassProperty {
						is_readonly: readonly_position.is_some(),
						position: *key.get_position(),
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
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Property(
				is_static,
				ClassProperty { is_readonly, key, type_annotation, value, position: _ },
			) => {
				if *is_static {
					buf.push_str("static ");
				}
				if *is_readonly {
					buf.push_str("readonly ");
				}
				key.to_string_from_buffer(buf, options, local);
				if let (true, Some(type_annotation)) = (options.include_types, type_annotation) {
					buf.push_str(": ");
					type_annotation.to_string_from_buffer(buf, options, local);
				}
				if let Some(value) = value {
					buf.push_str(if options.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, options, local);
				}
			}
			Self::Method(is_static, function) => {
				if *is_static {
					buf.push_str("static ");
				}
				function.to_string_from_buffer(buf, options, local.next_level());
			}
			Self::Constructor(constructor) => {
				constructor.to_string_from_buffer(buf, options, local.next_level());
			}
			Self::StaticBlock(block) => {
				buf.push_str("static ");
				block.to_string_from_buffer(buf, options, local.next_level());
			}
			Self::Comment(c, is_multiline, _) => {
				if options.should_add_comment(c.starts_with('.')) {
					if *is_multiline {
						buf.push_str("/*");
						buf.push_str(c);
						buf.push_str("*/");
					} else {
						buf.push_str("//");
						buf.push_str(c);
						buf.push_new_line();
					}
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
		get_set_generator: (Option<TokenStart>, MethodHeader),
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

	#[allow(clippy::similar_names)]
	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		// TODO not great
		let start = reader.peek().unwrap().1;
		let header = MethodHeader::from_reader(reader);
		let name = WithComment::<PropertyKey<_>>::from_reader(reader, state, options)?;
		Ok((((!header.is_no_modifiers()).then_some(start), header), name))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		header.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, options, local);
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit(visitors, data, options, chain);
	}

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit_mut(visitors, data, options, chain);
	}

	fn get_name(name: &Self::Name) -> Option<&str> {
		if let PropertyKey::Ident(name, ..) = name.get_ast_ref() {
			Some(name.as_str())
		} else {
			None
		}
	}
}

impl FunctionBased for ClassConstructorBase {
	type Header = ();
	type Name = ();
	type Body = Block;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassConstructor(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let start = state.expect_keyword(reader, TSXKeyword::Constructor)?;
		Ok(((Some(start), ()), ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		_header: &Self::Header,
		_name: &Self::Name,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		buf.push_str("constructor");
	}

	fn visit_name<TData>(
		(): &Self::Name,
		_: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn visit_name_mut<TData>(
		(): &mut Self::Name,
		_: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn get_name((): &Self::Name) -> Option<&str> {
		None
	}
}

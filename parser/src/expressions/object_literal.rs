use crate::{
	derive_ASTNode,
	errors::parse_lexing_error,
	functions::{FunctionBased, HeadingAndPosition, MethodHeader, ThisParameter},
	property_key::AlwaysPublic,
	throw_unexpected_token_with_token,
	visiting::Visitable,
	ASTNode, Block, Expression, FunctionBase, ParseOptions, ParseResult, PropertyKey, Span,
	TSXToken, Token, TokenReader, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use std::fmt::Debug;
use tokenizer_lib::sized_tokens::{TokenReaderWithTokenEnds, TokenStart};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ObjectLiteral {
	pub members: Vec<ObjectLiteralMember>,
	pub position: Span,
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, get_field_by_type::GetFieldByType)]
#[partial_eq_ignore_types(Span, VariableId)]
#[get_field_by_type_target(Span)]
pub enum ObjectLiteralMember {
	Spread(Expression, Span),
	Shorthand(String, Span),
	Property {
		key: WithComment<PropertyKey<AlwaysPublic>>,
		/// Makes object destructuring syntax a subset of object literal syntax
		assignment: bool,
		value: Expression,
		position: Span,
	},
	Method(ObjectLiteralMethod),
	Comment(String, bool, Span),
}

impl crate::Visitable for ObjectLiteralMember {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::Shorthand(..)
			| ObjectLiteralMember::Property { .. }
			| ObjectLiteralMember::Spread(..)
			| ObjectLiteralMember::Comment(..) => {}
			ObjectLiteralMember::Method(method) => method.visit(visitors, data, options, chain),
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		match self {
			ObjectLiteralMember::Shorthand(..)
			| ObjectLiteralMember::Property { .. }
			| ObjectLiteralMember::Spread(..)
			| ObjectLiteralMember::Comment(..) => {}
			ObjectLiteralMember::Method(method) => method.visit_mut(visitors, data, options, chain),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ObjectLiteralMethodBase;
pub type ObjectLiteralMethod = FunctionBase<ObjectLiteralMethodBase>;

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const OBJECT_LITERAL_METHOD_TYPE: &str = r"
	export interface ObjectLiteralMethod extends FunctionBase {
		header: MethodHeader,
		body: Block,
		name: WithComment<PropertyKey<AlwaysPublic>>,
		parameters: FunctionParameters<ThisParameter | null, null>
	}
";

impl FunctionBased for ObjectLiteralMethodBase {
	type Name = WithComment<PropertyKey<AlwaysPublic>>;
	type Header = MethodHeader;
	type Body = Block;
	type LeadingParameter = Option<ThisParameter>;
	type ParameterVisibility = ();

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		// TODO not great
		let start = reader.peek().unwrap().1;
		Ok((
			(Some(start), MethodHeader::from_reader(reader)),
			WithComment::from_reader(reader, state, options)?,
		))
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
		if let PropertyKey::Identifier(name, ..) = name.get_ast_ref() {
			Some(name.as_str())
		} else {
			None
		}
	}
}

impl Eq for ObjectLiteralMember {}

impl ASTNode for ObjectLiteral {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::OpenBrace)?;
		Self::from_reader_sub_open_curly(reader, state, options, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		crate::to_string_bracketed(&self.members, ('{', '}'), buf, options, local);
	}
}

impl ObjectLiteral {
	pub(crate) fn from_reader_sub_open_curly(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut members: Vec<ObjectLiteralMember> = Vec::new();
		loop {
			if matches!(reader.peek(), Some(Token(TSXToken::CloseBrace, _))) {
				break;
			}
			let member = ObjectLiteralMember::from_reader(reader, state, options)?;
			let is_comment = matches!(member, ObjectLiteralMember::Comment(..));
			members.push(member);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else if !is_comment {
				break;
			}
		}
		let end = reader.expect_next_get_end(TSXToken::CloseBrace)?;
		Ok(ObjectLiteral { members, position: start.union(end) })
	}
}

impl ASTNode for ObjectLiteralMember {
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

		if let Some(Token(_, spread_start)) =
			reader.conditional_next(|tok| matches!(tok, TSXToken::Spread))
		{
			// TODO precedence okay?
			let expression = Expression::from_reader(reader, state, options)?;
			let position = spread_start.union(expression.get_position());
			return Ok(Self::Spread(expression, position));
		};

		// TODO not great
		let start = reader.peek().unwrap().1;

		// Catch for named get or set :(
		let (header, key) = crate::functions::get_method_name(reader, state, options)?;

		let Token(token, _) = &reader.peek().ok_or_else(parse_lexing_error)?;
		match token {
			// Functions, (OpenChevron is for generic parameters)
			TSXToken::OpenParentheses | TSXToken::OpenChevron => {
				let method: ObjectLiteralMethod = FunctionBase::from_reader_with_header_and_name(
					reader,
					state,
					options,
					(Some(start), header),
					key,
				)?;

				Ok(Self::Method(method))
			}
			_ => {
				if !header.is_no_modifiers() {
					return crate::throw_unexpected_token(reader, &[TSXToken::OpenParentheses]);
				}
				if let Some(Token(TSXToken::Comma | TSXToken::CloseBrace, _)) = reader.peek() {
					if let PropertyKey::Identifier(name, position, _) = key.get_ast() {
						Ok(Self::Shorthand(name, position))
					} else {
						let token = reader.next().ok_or_else(parse_lexing_error)?;
						throw_unexpected_token_with_token(token, &[TSXToken::Colon])
					}
				} else {
					let token = reader.next().ok_or_else(parse_lexing_error)?;
					let assignment = match token.0 {
						TSXToken::Colon => false,
						TSXToken::Assign => true,
						_ => return throw_unexpected_token_with_token(token, &[TSXToken::Colon]),
					};
					// let assignment = if let
					let value = Expression::from_reader(reader, state, options)?;
					let position = key.get_position().union(value.get_position());
					Ok(Self::Property { assignment, key, value, position })
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Property { assignment: _, key, value, position: _ } => {
				key.to_string_from_buffer(buf, options, local);
				buf.push(':');
				options.push_gap_optionally(buf);
				value.to_string_from_buffer(buf, options, local);
			}
			Self::Shorthand(name, ..) => {
				buf.push_str(name.as_str());
			}
			Self::Method(func) => {
				func.to_string_from_buffer(buf, options, local);
			}
			Self::Spread(spread_expr, _) => {
				buf.push_str("...");
				spread_expr.to_string_from_buffer(buf, options, local);
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
		};
	}

	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}
}

use std::{fmt::Debug, marker::PhantomData};

use crate::property_key::PropertyKeyKind;
use crate::visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty};
use crate::{
	parse_bracketed, to_string_bracketed, ASTNode, Block, ExpressionOrStatementPosition,
	ExpressionPosition, GenericTypeConstraint, ParseOptions, ParseResult, TSXToken, TypeAnnotation,
	VisitOptions, Visitable, WithComment,
};
use crate::{PropertyKey, TSXKeyword};
use derive_partial_eq_extras::PartialEqExtras;
use source_map::{Span, ToString};
use tokenizer_lib::sized_tokens::TokenStart;
use tokenizer_lib::{Token, TokenReader};

pub use crate::parameters::*;

pub mod bases {
	pub use crate::{
		declarations::{
			classes::{ClassConstructorBase, ClassFunctionBase},
			StatementFunctionBase,
		},
		expressions::{
			arrow_function::ArrowFunctionBase, object_literal::ObjectLiteralMethodBase,
			ExpressionFunctionBase,
		},
	};
}

pub type HeadingAndPosition<T> = (Option<TokenStart>, <T as FunctionBased>::Header);

/// Specialization information for [`FunctionBase`]
pub trait FunctionBased: Debug + Clone + PartialEq + Eq + Send + Sync {
	/// Includes a keyword and/or modifiers
	type Header: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// A name of the function
	type Name: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// The body of the function
	type Body: ASTNode;

	/// For debugging only
	fn get_name(name: &Self::Name) -> Option<&str>;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)>;

	fn header_and_name_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	);

	/// For [`crate::ArrowFunction`]
	#[must_use]
	fn get_parameter_body_boundary_token() -> Option<TSXToken> {
		None
	}

	/// For [`crate::ArrowFunction`]
	fn parameters_from_reader<T: ToString>(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<FunctionParameters> {
		FunctionParameters::from_reader(reader, state, options)
	}

	/// For [`crate::ArrowFunction`]
	fn parameters_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		parameters: &FunctionParameters,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		parameters.to_string_from_buffer(buf, options, local);
	}

	/// For [`crate::ArrowFunction`]
	fn parameter_body_boundary_token_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		options: &crate::ToStringOptions,
	) {
		options.add_gap(buf);
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	);

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	);
}

/// Base for all function based structures with bodies (no interface, type reference etc)
///
/// Note: the [`PartialEq`] implementation is based on syntactical representation rather than [`FunctionId`] equality
#[derive(Debug, Clone, PartialEqExtras, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct FunctionBase<T: FunctionBased> {
	pub header: T::Header,
	pub name: T::Name,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub parameters: FunctionParameters,
	pub return_type: Option<TypeAnnotation>,
	pub body: T::Body,
	pub position: Span,
}

impl<T: FunctionBased> Eq for FunctionBase<T> {}

impl<T: FunctionBased + 'static> ASTNode for FunctionBase<T> {
	#[allow(clippy::similar_names)]
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let (header_and_left, name) = T::header_and_name_from_reader(reader, state, options)?;
		Self::from_reader_with_header_and_name(reader, state, options, header_and_left, name)
	}

	fn to_string_from_buffer<TS: source_map::ToString>(
		&self,
		buf: &mut TS,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		T::header_and_name_to_string_from_buffer(buf, &self.header, &self.name, options, local);
		if let (true, Some(type_parameters)) = (options.include_types, &self.type_parameters) {
			to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
		}
		T::parameters_to_string_from_buffer(buf, &self.parameters, options, local);
		if let (true, Some(return_type)) = (options.include_types, &self.return_type) {
			buf.push_str(": ");
			return_type.to_string_from_buffer(buf, options, local);
		}
		T::parameter_body_boundary_token_to_string_from_buffer(buf, options);
		self.body.to_string_from_buffer(buf, options, local.next_level());
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

#[allow(clippy::similar_names)]
impl<T: FunctionBased> FunctionBase<T> {
	pub(crate) fn from_reader_with_header_and_name(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		(header_left, header): (Option<TokenStart>, T::Header),
		name: T::Name,
	) -> ParseResult<Self> {
		let type_parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
					.map(|(params, _)| params)
			})
			.transpose()?;
		let parameters = FunctionParameters::from_reader(reader, state, options)?;
		let return_type = reader
			.conditional_next(|tok| options.type_annotations && matches!(tok, TSXToken::Colon))
			.is_some()
			.then(|| TypeAnnotation::from_reader(reader, state, options))
			.transpose()?;

		if let Some(token) = T::get_parameter_body_boundary_token() {
			reader.expect_next(token)?;
		}
		let body = T::Body::from_reader(reader, state, options)?;
		let body_pos = body.get_position();
		let position =
			header_left.unwrap_or_else(|| parameters.position.get_start()).union(body_pos);
		Ok(Self { header, name, type_parameters, parameters, return_type, body, position })
	}
}

/// Visiting logic: TODO make visiting macro better and remove
impl<T: FunctionBased> Visitable for FunctionBase<T>
where
	T::Body: Visitable,
	// T::Header: Visitable,
{
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		// Don't think there is anything useful about visiting header
		// self.header.visit(visitors, data, options, chain);
		T::visit_name(&self.name, visitors, data, options, chain);
		if options.visit_nested_blocks || chain.is_empty() {
			self.parameters.visit(visitors, data, options, chain);
			self.body.visit(visitors, data, options, chain);
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		// Don't think there is anything useful about visiting header
		// self.header.visit_mut(visitors, data, options, chain);
		T::visit_name_mut(&mut self.name, visitors, data, options, chain);
		if options.visit_nested_blocks || chain.is_empty() {
			self.parameters.visit_mut(visitors, data, options, chain);
			self.body.visit_mut(visitors, data, options, chain);
		}
	}
}

/// Base for all functions with the `function` keyword
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneralFunctionBase<T: ExpressionOrStatementPosition>(PhantomData<T>);

pub type ExpressionFunction = FunctionBase<GeneralFunctionBase<ExpressionPosition>>;

#[allow(clippy::similar_names)]
impl<T: ExpressionOrStatementPosition> FunctionBased for GeneralFunctionBase<T> {
	type Body = Block;
	type Header = FunctionHeader;
	type Name = T;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let header = FunctionHeader::from_reader(reader, state, options)?;
		let name = T::from_reader(reader, state, options)?;
		Ok(((Some(header.get_position().get_start()), header), name))
	}

	fn header_and_name_to_string_from_buffer<U: source_map::ToString>(
		buf: &mut U,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		header.to_string_from_buffer(buf, options, local);
		if let Some(name) = name.as_option_str() {
			buf.push_str(name);
		}
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable(
			&ImmutableVariableOrProperty::FunctionName(name.as_option_variable_identifier()),
			data,
			chain,
		);
	}

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable_mut(
			&mut MutableVariableOrProperty::FunctionName(name.as_option_variable_identifier_mut()),
			data,
			chain,
		);
	}

	fn get_name(name: &Self::Name) -> Option<&str> {
		name.as_option_str()
	}
}

#[cfg(feature = "extras")]
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum FunctionLocationModifier {
	Server,
	Worker,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum FunctionHeader {
	VirginFunctionHeader {
		is_async: bool,
		#[cfg(feature = "extras")]
		location: Option<FunctionLocationModifier>,
		generator_star_token_position: Option<Span>,
		position: Span,
	},
	#[cfg(feature = "extras")]
	ChadFunctionHeader {
		is_async: bool,
		is_generator: bool,
		location: Option<FunctionLocationModifier>,
		position: Span,
	},
}

impl ASTNode for FunctionHeader {
	fn get_position(&self) -> &Span {
		match self {
			FunctionHeader::VirginFunctionHeader { position, .. } => position,
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { position, .. } => position,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<Self> {
		let async_start =
			state.optionally_expect_keyword(reader, TSXKeyword::Async).map(|kw| kw.get_start());
		parse_special_then_regular_header(reader, state, async_start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		if self.is_async() {
			buf.push_str("async ");
		}
		buf.push_str("function");
		if self.is_generator() {
			buf.push_str("* ");
		} else {
			buf.push(' ');
		}
	}
}

pub(crate) fn parse_special_then_regular_header(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	state: &mut crate::ParsingState,
	async_kw_pos: Option<TokenStart>,
) -> Result<FunctionHeader, crate::ParseError> {
	#[cfg(feature = "extras")]
	{
		let next_generator = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(crate::TSXKeyword::Generator)));

		if let Some(token) = next_generator {
			let span = token.get_span();
			let location = parse_function_location(reader);

			let pos = state.expect_keyword_get_full_span(reader, TSXKeyword::Function)?;
			let position = span.union(pos);

			Ok(FunctionHeader::ChadFunctionHeader {
				is_async: async_kw_pos.is_some(),
				location,
				is_generator: true,
				position,
			})
		} else {
			parse_regular_header(reader, state, async_kw_pos)
		}
	}

	#[cfg(not(feature = "extras"))]
	parse_regular_header(reader, state, async_kw_pos)
}

#[cfg(feature = "extras")]
pub(crate) fn parse_function_location(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
) -> Option<FunctionLocationModifier> {
	if let Some(Token(TSXToken::Keyword(TSXKeyword::Server | TSXKeyword::Worker), _)) =
		reader.peek()
	{
		Some(match reader.next().unwrap() {
			Token(TSXToken::Keyword(TSXKeyword::Server), _) => FunctionLocationModifier::Server,
			Token(TSXToken::Keyword(TSXKeyword::Worker), _) => FunctionLocationModifier::Worker,
			_ => unreachable!(),
		})
	} else {
		None
	}
}

fn parse_regular_header(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	state: &mut crate::ParsingState,
	async_kw_pos: Option<TokenStart>,
) -> Result<FunctionHeader, crate::ParseError> {
	#[cfg(feature = "extras")]
	let location = parse_function_location(reader);

	let function_start = state.expect_keyword(reader, TSXKeyword::Function)?;
	let is_async = async_kw_pos.is_some();

	let generator_star_token_position = reader
		.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
		.map(|token| token.get_span());

	let start = async_kw_pos.unwrap_or(function_start);

	let position = if let Some(ref generator_star_token_position) = generator_star_token_position {
		start.union(generator_star_token_position)
	} else {
		function_start.with_length(TSXKeyword::Function.length() as usize)
	};

	Ok(FunctionHeader::VirginFunctionHeader {
		is_async,
		generator_star_token_position,
		position,
		#[cfg(feature = "extras")]
		location,
	})
}

impl FunctionHeader {
	#[must_use]
	pub fn is_generator(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader {
				generator_star_token_position: generator_star_token_pos,
				..
			} => generator_star_token_pos.is_some(),
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { is_generator, .. } => *is_generator,
		}
	}

	#[must_use]
	pub fn is_async(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader { is_async, .. } => *is_async,
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { is_async, .. } => *is_async,
		}
	}

	#[cfg(feature = "extras")]
	#[must_use]
	pub fn get_location(&self) -> Option<&FunctionLocationModifier> {
		match self {
			FunctionHeader::VirginFunctionHeader { location, .. }
			| FunctionHeader::ChadFunctionHeader { location, .. } => location.as_ref(),
		}
	}
}

/// This structure removes possible invalid combinations with async
#[derive(Eq, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum MethodHeader {
	Get,
	Set,
	Regular { is_async: bool, generator: Option<GeneratorSpecifier> },
}

impl Default for MethodHeader {
	fn default() -> Self {
		Self::Regular { is_async: false, generator: None }
	}
}

impl MethodHeader {
	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(&self, buf: &mut T) {
		match self {
			MethodHeader::Get => buf.push_str("get "),
			MethodHeader::Set => buf.push_str("set "),
			MethodHeader::Regular { is_async, generator } => {
				if *is_async {
					buf.push_str("async ");
				}
				if let Some(_generator) = generator {
					buf.push('*');
				}
			}
		}
	}

	pub(crate) fn from_reader(reader: &mut impl TokenReader<TSXToken, crate::TokenStart>) -> Self {
		match reader.peek() {
			Some(Token(TSXToken::Keyword(TSXKeyword::Get), _)) => {
				let _ = reader.next();
				MethodHeader::Get
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::Set), _)) => {
				let _ = reader.next();
				MethodHeader::Set
			}
			_ => {
				let is_async = reader
					.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Async)))
					.is_some();

				let generator = GeneratorSpecifier::from_reader(reader);
				MethodHeader::Regular { is_async, generator }
			}
		}
	}

	#[must_use]
	pub fn is_async(&self) -> bool {
		matches!(self, Self::Regular { is_async: true, .. })
	}

	#[must_use]
	pub fn is_generator(&self) -> bool {
		matches!(self, Self::Regular { generator: Some(_), .. })
	}

	#[must_use]
	pub fn is_no_modifiers(&self) -> bool {
		matches!(self, Self::Regular { is_async: false, generator: None })
	}
}

#[derive(Eq, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum GeneratorSpecifier {
	Star(Span),
	#[cfg(feature = "extras")]
	Keyword,
}

impl GeneratorSpecifier {
	pub(crate) fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> Option<Self> {
		match reader.peek() {
			Some(Token(TSXToken::Multiply, _)) => {
				Some(GeneratorSpecifier::Star(reader.next().unwrap().get_span()))
			}
			#[cfg(feature = "extras")]
			Some(Token(TSXToken::Keyword(TSXKeyword::Generator), _)) => Some(GeneratorSpecifier::Keyword),
			_ => None,
		}
	}
}

/// Accounts for methods named `get` and `set` etc
pub(crate) fn get_method_name<T: PropertyKeyKind + 'static>(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
) -> Result<(MethodHeader, WithComment<PropertyKey<T>>), crate::ParseError> {
	let is_named_get_set_or_async = matches!(
		reader.peek(),
		Some(Token(TSXToken::Keyword(TSXKeyword::Get | TSXKeyword::Set | TSXKeyword::Async), _))
	) && matches!(
		reader.peek_n(1),
		Some(Token(
			TSXToken::OpenParentheses
				| TSXToken::Colon
				| TSXToken::OpenChevron
				| TSXToken::CloseBrace
				| TSXToken::Comma,
			_
		))
	);

	let (function_header, key) = if is_named_get_set_or_async {
		let token = reader.next().unwrap();
		let position = token.get_span();
		let name = match token.0 {
			TSXToken::Keyword(TSXKeyword::Get) => "get",
			TSXToken::Keyword(TSXKeyword::Set) => "set",
			TSXToken::Keyword(TSXKeyword::Async) => "async",
			_ => unreachable!(),
		};
		// TODO
		let new_public = T::new_public();
		(
			MethodHeader::default(),
			WithComment::None(PropertyKey::Ident(name.to_owned(), position, new_public)),
		)
	} else {
		(MethodHeader::from_reader(reader), WithComment::from_reader(reader, state, options)?)
	};
	Ok((function_header, key))
}

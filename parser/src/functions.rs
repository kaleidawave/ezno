use std::{fmt::Debug, marker::PhantomData};

use crate::visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty};
use crate::{
	parse_bracketed, to_string_bracketed, ASTNode, Block, ExpressionOrStatementPosition,
	ExpressionPosition, GenericTypeConstraint, Keyword, ParseOptions, ParseResult, TSXToken,
	TypeAnnotation, VisitOptions, Visitable,
};
use crate::{tsx_keywords, TSXKeyword};
use derive_partial_eq_extras::PartialEqExtras;
use source_map::{Span, ToString};
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

/// Specialization information for [`FunctionBase`]
pub trait FunctionBased: Debug + Clone + PartialEq + Eq + Send + Sync {
	/// Includes a keyword and/or modifiers
	type Header: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// A name of the function
	type Name: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// The body of the function
	type Body: ASTNode;

	fn header_left(header: &Self::Header) -> Option<source_map::Start>;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)>;

	fn header_and_name_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		depth: u8,
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
		depth: u8,
	) {
		parameters.to_string_from_buffer(buf, options, depth);
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
		let (header, name) = T::header_and_name_from_reader(reader, state, options)?;
		Self::from_reader_with_header_and_name(reader, state, options, header, name)
	}

	fn to_string_from_buffer<TS: source_map::ToString>(
		&self,
		buf: &mut TS,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		T::header_and_name_to_string_from_buffer(buf, &self.header, &self.name, options, depth);
		if let (true, Some(type_parameters)) = (options.include_types, &self.type_parameters) {
			to_string_bracketed(type_parameters, ('<', '>'), buf, options, depth);
		}
		T::parameters_to_string_from_buffer(buf, &self.parameters, options, depth);
		if let (true, Some(return_type)) = (options.include_types, &self.return_type) {
			buf.push_str(": ");
			return_type.to_string_from_buffer(buf, options, depth);
		}
		T::parameter_body_boundary_token_to_string_from_buffer(buf, options);
		self.body.to_string_from_buffer(buf, options, depth + 1);
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
		header: T::Header,
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
			.conditional_next(|tok| matches!(tok, TSXToken::Colon))
			.is_some()
			.then(|| TypeAnnotation::from_reader(reader, state, options))
			.transpose()?;

		if let Some(token) = T::get_parameter_body_boundary_token() {
			reader.expect_next(token)?;
		}
		let body = T::Body::from_reader(reader, state, options)?;
		let body_pos = body.get_position();
		let position = if let Some(header_pos) = T::header_left(&header) {
			header_pos.union(body_pos)
		} else {
			parameters.position.clone().union(body_pos)
		};
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
	) -> ParseResult<(Self::Header, Self::Name)> {
		let header = FunctionHeader::from_reader(reader, state, options)?;
		let name = T::from_reader(reader, state, options)?;
		Ok((header, name))
	}

	fn header_and_name_to_string_from_buffer<U: source_map::ToString>(
		buf: &mut U,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		header.to_string_from_buffer(buf, options, depth);
		if let Some(name) = name.as_option_str() {
			buf.push_str(name);
		}
	}

	fn header_left(header: &Self::Header) -> Option<source_map::Start> {
		Some(header.get_position().get_start())
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
}

#[cfg(feature = "extras")]
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum FunctionLocationModifier {
	Server(Keyword<tsx_keywords::Server>),
	Worker(Keyword<tsx_keywords::Worker>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum FunctionHeader {
	VirginFunctionHeader {
		async_keyword: Option<Keyword<tsx_keywords::Async>>,
		#[cfg(feature = "extras")]
		location: Option<FunctionLocationModifier>,
		function_keyword: Keyword<tsx_keywords::Function>,
		generator_star_token_position: Option<Span>,
		position: Span,
	},
	#[cfg(feature = "extras")]
	ChadFunctionHeader {
		async_keyword: Option<Keyword<tsx_keywords::Async>>,
		generator_keyword: Option<Keyword<tsx_keywords::Generator>>,
		location: Option<FunctionLocationModifier>,
		function_keyword: Keyword<tsx_keywords::Function>,
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
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<Self> {
		let async_keyword = Keyword::optionally_from_reader(reader);

		function_header_from_reader_with_async_keyword(reader, async_keyword)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_options: &crate::ToStringOptions,
		_depth: u8,
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

pub(crate) fn function_header_from_reader_with_async_keyword(
	reader: &mut impl TokenReader<TSXToken, source_map::Start>,
	async_keyword: Option<Keyword<tsx_keywords::Async>>,
) -> Result<FunctionHeader, crate::ParseError> {
	#[cfg(feature = "extras")]
	{
		let next_generator = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(crate::TSXKeyword::Generator)));

		if let Some(token) = next_generator {
			let span = token.get_span();
			let generator_keyword = Some(Keyword::new(span));
			let location = parse_function_location(reader);

			let function_keyword = Keyword::from_reader(reader)?;
			let position = async_keyword
				.as_ref()
				.map_or(&span, |kw| kw.get_position())
				.union(function_keyword.get_position());

			Ok(FunctionHeader::ChadFunctionHeader {
				async_keyword,
				location,
				generator_keyword,
				function_keyword,
				position,
			})
		} else {
			parse_regular_header(reader, async_keyword)
		}
	}

	#[cfg(not(feature = "extras"))]
	parse_regular_header(reader, async_keyword)
}

#[cfg(feature = "extras")]
pub(crate) fn parse_function_location(
	reader: &mut impl TokenReader<TSXToken, source_map::Start>,
) -> Option<FunctionLocationModifier> {
	if let Some(Token(TSXToken::Keyword(TSXKeyword::Server | TSXKeyword::Worker), _)) =
		reader.peek()
	{
		Some(match reader.next().unwrap() {
			t @ Token(TSXToken::Keyword(TSXKeyword::Server), _) => {
				FunctionLocationModifier::Server(Keyword::new(t.get_span()))
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Worker), _) => {
				FunctionLocationModifier::Worker(Keyword::new(t.get_span()))
			}
			_ => unreachable!(),
		})
	} else {
		None
	}
}

fn parse_regular_header(
	reader: &mut impl TokenReader<TSXToken, source_map::Start>,
	async_keyword: Option<Keyword<tsx_keywords::Async>>,
) -> Result<FunctionHeader, crate::ParseError> {
	#[cfg(feature = "extras")]
	let location = parse_function_location(reader);

	let function_keyword = Keyword::from_reader(reader)?;
	let generator_star_token_position = reader
		.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
		.map(|token| token.get_span());

	let mut position =
		*async_keyword.as_ref().map_or(function_keyword.get_position(), |kw| kw.get_position());

	if let Some(ref generator_star_token_position) = generator_star_token_position {
		position = position.union(generator_star_token_position);
	}

	Ok(FunctionHeader::VirginFunctionHeader {
		async_keyword,
		function_keyword,
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
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { generator_keyword, .. } => generator_keyword.is_some(),
		}
	}

	#[must_use]
	pub fn is_async(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader { async_keyword, .. } => async_keyword.is_some(),
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { async_keyword, .. } => async_keyword.is_some(),
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
	Get(Keyword<tsx_keywords::Get>),
	Set(Keyword<tsx_keywords::Set>),
	Regular { r#async: Option<Keyword<tsx_keywords::Async>>, generator: Option<GeneratorSpecifier> },
}

impl Default for MethodHeader {
	fn default() -> Self {
		Self::Regular { r#async: None, generator: None }
	}
}

impl MethodHeader {
	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(&self, buf: &mut T) {
		match self {
			MethodHeader::Get(_) => buf.push_str("get "),
			MethodHeader::Set(_) => buf.push_str("set "),
			MethodHeader::Regular { r#async, generator } => {
				if r#async.is_some() {
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
				MethodHeader::Get(Keyword::new(reader.next().unwrap().get_span()))
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::Set), _)) => {
				MethodHeader::Set(Keyword::new(reader.next().unwrap().get_span()))
			}
			_ => {
				let r#async = reader
					.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Async)))
					.map(|tok| Keyword::new(tok.get_span()));

				let generator = GeneratorSpecifier::from_reader(reader);

				MethodHeader::Regular { r#async, generator }
			}
		}
	}

	pub(crate) fn get_start(&self) -> Option<source_map::Start> {
		match self {
			MethodHeader::Get(kw) => Some(kw.1.get_start()),
			MethodHeader::Set(kw) => Some(kw.1.get_start()),
			MethodHeader::Regular { r#async: Some(r#async), .. } => {
				Some(r#async.get_position().get_start())
			}
			MethodHeader::Regular { generator: Some(generator), .. } => Some(generator.get_start()),
			MethodHeader::Regular { .. } => None,
		}
	}

	#[must_use]
	pub fn is_async(&self) -> bool {
		matches!(self, Self::Regular { r#async: Some(_), .. })
	}

	#[must_use]
	pub fn is_generator(&self) -> bool {
		matches!(self, Self::Regular { generator: Some(_), .. })
	}

	#[must_use]
	pub fn is_some(&self) -> bool {
		!matches!(self, Self::Regular { r#async: None, generator: None })
	}
}

#[derive(Eq, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum GeneratorSpecifier {
	Star(Span),
	#[cfg(feature = "extras")]
	Keyword(Keyword<tsx_keywords::Generator>),
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
			Some(Token(TSXToken::Keyword(TSXKeyword::Generator), _)) => {
				Some(GeneratorSpecifier::Keyword(Keyword::new(reader.next().unwrap().get_span())))
			}
			_ => None,
		}
	}

	fn get_start(&self) -> source_map::Start {
		match self {
			GeneratorSpecifier::Star(pos) => pos.get_start(),
			#[cfg(feature = "extras")]
			GeneratorSpecifier::Keyword(kw) => kw.get_position().get_start(),
		}
	}
}

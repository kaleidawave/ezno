use std::{
	borrow::Cow,
	fmt::Debug,
	marker::PhantomData,
	sync::atomic::{AtomicU16, Ordering},
};

use crate::{
	extractor::ExtractedFunctions, parameters::*, parse_bracketed, to_string_bracketed, ASTNode,
	Block, ChainVariable, ExpressionOrStatementPosition, ExpressionPosition, GenericTypeConstraint,
	Keyword, ParseResult, ParseSettings, TSXToken, TypeReference, VisitSettings, Visitable,
};
use crate::{tsx_keywords, TSXKeyword};
use derive_debug_extras::DebugExtras;
use derive_partial_eq_extras::PartialEqExtras;
use source_map::{Span, ToString};
use tokenizer_lib::{Token, TokenReader};

pub mod bases {
	pub use crate::{
		expressions::{
			arrow_function::ArrowFunctionBase, object_literal::ObjectLiteralMethodBase,
			ExpressionFunctionBase,
		},
		statements::{
			classes::{ClassConstructorBase, ClassFunctionBase},
			StatementFunctionBase,
		},
	};
}

static FUNCTION_ID_COUNTER: AtomicU16 = AtomicU16::new(1);

/// Id given to AST that declares a function
#[derive(PartialEq, Eq, Clone, DebugExtras, Hash)]
pub struct FunctionId<T: FunctionBased>(u16, PhantomData<T>);

impl<T: FunctionBased> Copy for FunctionId<T> {}

// TODO better than global counter
impl<T: FunctionBased> FunctionId<T> {
	pub fn new() -> Self {
		Self(FUNCTION_ID_COUNTER.fetch_add(1, Ordering::SeqCst), Default::default())
	}

	pub fn get_id(self) -> u16 {
		self.0
	}

	pub fn from_id(value: u16) -> Self {
		Self(value, Default::default())
	}
}

/// Specialization information for [FunctionBase]
pub trait FunctionBased: Debug + Clone + PartialEq + Eq + Send + Sync {
	/// Includes a keyword and/or modifiers
	type Header: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// A name of the function
	type Name: Debug + Clone + PartialEq + Eq + Send + Sync;
	/// The body of the function
	type Body: ASTNode;

	fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable;

	fn header_left(header: &Self::Header) -> Option<Cow<Span>>;

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<(Self::Header, Self::Name)>;

	fn header_and_name_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	);

	/// For [crate::ArrowFunction]
	fn get_parameter_body_boundary_token() -> Option<TSXToken> {
		None
	}

	/// For [crate::ArrowFunction]
	fn parameters_from_reader<T: ToString>(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<FunctionParameters> {
		FunctionParameters::from_reader(reader, state, settings)
	}

	/// For [crate::ArrowFunction]
	fn parameters_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		parameters: &FunctionParameters,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		parameters.to_string_from_buffer(buf, settings, depth);
	}

	/// For [crate::ArrowFunction]
	fn parameter_body_boundary_token_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
	) {
		settings.0.add_gap(buf);
	}
}

/// Base for all function based structures with bodies (no interface, type reference etc)
///
/// Note: the [PartialEq] implementation is based on syntactical representation rather than [FunctionId] equality
#[derive(Debug, Clone, PartialEqExtras)]
pub struct FunctionBase<T: FunctionBased> {
	/// TODO should be private
	#[partial_eq_ignore]
	pub function_id: FunctionId<T>,
	pub header: T::Header,
	pub name: T::Name,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub parameters: FunctionParameters,
	pub return_type: Option<TypeReference>,
	pub body: T::Body,
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: FunctionBased> self_rust_tokenize::SelfRustTokenize for FunctionBase<T> {
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!("fb to tokens")
	}
}

impl<T: FunctionBased> Eq for FunctionBase<T> {}

impl<T: FunctionBased + 'static> ASTNode for FunctionBase<T> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let (header, name) = T::header_and_name_from_reader(reader, state, settings)?;
		Self::from_reader_with_header_and_name(reader, state, settings, header, name)
	}

	fn to_string_from_buffer<TS: source_map::ToString>(
		&self,
		buf: &mut TS,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		T::header_and_name_to_string_from_buffer(buf, &self.header, &self.name, settings, depth);
		if let (true, Some(type_parameters)) = (settings.0.include_types, &self.type_parameters) {
			to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth);
		}
		T::parameters_to_string_from_buffer(buf, &self.parameters, settings, depth);
		if let (true, Some(return_type)) = (settings.0.include_types, &self.return_type) {
			buf.push_str(": ");
			return_type.to_string_from_buffer(buf, settings, depth);
		}
		T::parameter_body_boundary_token_to_string_from_buffer(buf, settings);
		self.body.to_string_from_buffer(buf, settings, depth + 1);
	}

	fn get_position(&self) -> Cow<Span> {
		let body_pos = self.body.get_position();
		if let Some(header_pos) = T::header_left(&self.header) {
			Cow::Owned(header_pos.union(&body_pos))
		} else {
			Cow::Owned(self.parameters.position.clone().union(&body_pos))
		}
	}
}

impl<T: FunctionBased> FunctionBase<T> {
	pub(crate) fn from_reader_with_header_and_name(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		header: T::Header,
		name: T::Name,
	) -> ParseResult<Self> {
		let type_parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)
					.map(|(params, _)| params)
			})
			.transpose()?;

		let parameters = FunctionParameters::from_reader(reader, state, settings)?;
		let return_type = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Colon))
			.is_some()
			.then(|| TypeReference::from_reader(reader, state, settings))
			.transpose()?;

		if let Some(token) = T::get_parameter_body_boundary_token() {
			reader.expect_next(token)?;
		}
		let body = T::Body::from_reader(reader, state, settings)?;
		Ok(Self {
			header,
			name,
			parameters,
			type_parameters,
			body,
			return_type,
			function_id: FunctionId::new(),
		})
	}

	pub fn get_function_id(&self) -> FunctionId<T> {
		self.function_id
	}
}

/// Visiting logic: TODO make visiting macro better and remove
impl<T: FunctionBased> Visitable for FunctionBase<T>
where
	T::Body: Visitable,
{
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.parameters.visit(visitors, data, settings, functions, chain);
		if settings.visit_function_bodies {
			self.body.visit(visitors, data, settings, functions, chain);
		}
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.parameters.visit_mut(visitors, data, settings, functions, chain);
		if settings.visit_function_bodies {
			self.body.visit_mut(visitors, data, settings, functions, chain);
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneralFunctionBase<T: ExpressionOrStatementPosition>(PhantomData<T>);

pub type ExpressionFunction = FunctionBase<GeneralFunctionBase<ExpressionPosition>>;

impl<T: ExpressionOrStatementPosition> FunctionBased for GeneralFunctionBase<T> {
	type Body = Block;
	type Header = FunctionHeader;
	type Name = T::Name;

	fn get_chain_variable(_this: &FunctionBase<Self>) -> crate::ChainVariable {
		todo!()
		// crate::ChainVariable::UnderExpressionFunctionBlock(self.base.body.1, self.expression_id)
	}

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let header = FunctionHeader::from_reader(reader, state, settings)?;
		let (name, _) = T::from_reader(reader, state, settings)?;
		Ok((header, name))
	}

	fn header_and_name_to_string_from_buffer<U: source_map::ToString>(
		buf: &mut U,
		header: &Self::Header,
		name: &Self::Name,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		header.to_string_from_buffer(buf, settings, depth);
		if let Some(name) = T::as_option_str(name) {
			buf.push_str(name);
		}
	}

	fn header_left(header: &Self::Header) -> Option<Cow<Span>> {
		Some(header.get_position())
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionHeader {
	VirginFunctionHeader {
		async_keyword: Option<Keyword<tsx_keywords::Async>>,
		function_keyword: Keyword<tsx_keywords::Function>,
		generator_star_token_pos: Option<Span>,
	},
	#[cfg(feature = "extras")]
	ChadFunctionHeader {
		async_keyword: Option<Keyword<tsx_keywords::Async>>,
		generator_keyword: Option<Keyword<tsx_keywords::Generator>>,
		function_keyword: Keyword<tsx_keywords::Function>,
	},
}

impl ASTNode for FunctionHeader {
	fn get_position(&self) -> Cow<Span> {
		match self {
			FunctionHeader::VirginFunctionHeader {
				async_keyword,
				function_keyword,
				generator_star_token_pos,
			} => {
				let mut position = Cow::Borrowed(
					async_keyword.as_ref().map(|kw| &kw.1).unwrap_or(&function_keyword.1),
				);
				if let Some(generator_star_token_pos) = generator_star_token_pos {
					position = Cow::Owned(position.union(generator_star_token_pos));
				}
				position
			}
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader {
				async_keyword,
				generator_keyword,
				function_keyword,
			} => {
				let first_pos = async_keyword
					.as_ref()
					.map(Keyword::get_position)
					.or_else(|| generator_keyword.as_ref().map(Keyword::get_position));

				if let Some(first_pos) = first_pos {
					Cow::Owned(first_pos.union(&function_keyword.1))
				} else {
					Cow::Borrowed(&function_keyword.1)
				}
			}
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseSettings,
	) -> ParseResult<Self> {
		let async_keyword = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Async)))
			.map(|Token(_, span)| Keyword::new(span));

		let next_generator =
			reader.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Generator)));
		if let Some(Token(_, span)) = next_generator {
			let generator_keyword = Some(Keyword::new(span));
			let span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Function))?;
			let function_keyword = Keyword::new(span);
			Ok(Self::ChadFunctionHeader { async_keyword, generator_keyword, function_keyword })
		} else {
			let span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Function))?;
			let function_keyword = Keyword::new(span);
			let generator_star_token_pos = reader
				.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
				.map(|Token(_, pos)| pos);

			Ok(Self::VirginFunctionHeader {
				async_keyword,
				function_keyword,
				generator_star_token_pos,
			})
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
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

impl FunctionHeader {
	pub fn is_generator(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader { generator_star_token_pos, .. } => {
				generator_star_token_pos.is_some()
			}
			FunctionHeader::ChadFunctionHeader { generator_keyword, .. } => {
				generator_keyword.is_some()
			}
		}
	}

	pub fn is_async(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader { async_keyword, .. }
			| FunctionHeader::ChadFunctionHeader { async_keyword, .. } => async_keyword.is_some(),
		}
	}
}

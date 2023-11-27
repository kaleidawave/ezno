#![doc = include_str!("../README.md")]
#![deny(clippy::all)]
#![allow(clippy::new_without_default)]

mod block;
mod comments;
pub mod cursor;
pub mod declarations;
mod errors;
pub mod expressions;
mod extensions;
pub mod functions;
pub mod generator_helpers;
mod lexer;
mod modules;
pub mod operators;
pub mod parameters;
pub mod property_key;
pub mod statements;
mod tokens;
pub mod types;
mod variable_fields;
pub mod visiting;

pub use block::{Block, BlockLike, BlockLikeMut, BlockOrSingleStatement, StatementOrDeclaration};
pub use comments::WithComment;
pub use cursor::{CursorId, EmptyCursorId};
pub use declarations::Declaration;

use errors::parse_lexing_error;
pub use errors::{ParseError, ParseErrors, ParseResult};
pub use expressions::{Expression, PropertyReference};
pub use extensions::{
	decorators::{Decorated, Decorator},
	is_expression,
	jsx::*,
};
pub use functions::{FunctionBase, FunctionBased, FunctionHeader};
pub use generator_helpers::IntoAST;
use iterator_endiate::EndiateIteratorExt;
pub use lexer::{lex_script, LexerOptions};
pub use modules::{FromFileError, Module, TypeDefinitionModule, TypeDefinitionModuleDeclaration};
pub use parameters::{FunctionParameters, Parameter, SpreadParameter};
pub use property_key::PropertyKey;
pub use source_map::{self, SourceId, Span};
pub use statements::Statement;
use temporary_annex::Annex;
pub use tokens::{tsx_keywords, TSXKeyword, TSXToken};
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, GenericTypeConstraint, TypeDeclaration},
};
pub use variable_fields::*;
pub(crate) use visiting::*;

use tokenizer_lib::{sized_tokens::TokenEnd, Token, TokenReader};

pub(crate) use tokenizer_lib::sized_tokens::TokenStart;

use std::{borrow::Cow, ops::Neg, str::FromStr};

/// The notation of a string
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum Quoted {
	Single,
	Double,
}

impl Quoted {
	fn as_char(self) -> char {
		match self {
			Quoted::Single => '\'',
			Quoted::Double => '"',
		}
	}
}

/// Settings to customize parsing
#[allow(unused)]
#[derive(Copy, Clone)]
pub struct ParseOptions {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	/// Allow custom characters in JSX attributes
	pub special_jsx_attributes: bool,
	/// Parses decorators on items
	pub decorators: bool,
	pub generator_keyword: bool,
	/// Skip **all** comments from the AST
	pub include_comments: bool,
	/// See [crate::extensions::is_expression::IsExpression]
	pub is_expressions: bool,
	/// Allows functions to be prefixed with 'server'
	pub server_functions: bool,
	/// Allows functions to be prefixed with 'module'
	pub module_functions: bool,
	/// For LSP allows incomplete AST for completions. TODO tidy up
	pub slots: bool,
}

impl ParseOptions {
	fn get_lex_options(&self) -> LexerOptions {
		LexerOptions {
			include_comments: self.include_comments,
			lex_jsx: self.jsx,
			allow_unsupported_characters_in_jsx_attribute_keys: self.special_jsx_attributes,
		}
	}

	pub fn all_features() -> Self {
		Self {
			jsx: true,
			special_jsx_attributes: true,
			include_comments: true,
			decorators: true,
			slots: true,
			generator_keyword: true,
			server_functions: true,
			module_functions: true,
			is_expressions: true,
		}
	}
}

// TODO not sure about some of these defaults, may change in future
impl Default for ParseOptions {
	fn default() -> Self {
		Self {
			jsx: true,
			special_jsx_attributes: false,
			include_comments: true,
			decorators: true,
			slots: false,
			generator_keyword: true,
			server_functions: false,
			module_functions: false,
			is_expressions: true,
		}
	}
}

/// Settings for serializing ASTNodes
pub struct ToStringOptions {
	/// Does not include whitespace minification
	pub pretty: bool,
	/// Blocks have trailing semicolons. Has no effect if pretty == false
	pub trailing_semicolon: bool,
	/// Include type annotation syntax
	pub include_types: bool,
	/// TODO not sure about this
	pub include_decorators: bool,
	pub include_comments: bool,
	pub indent_with: String,
	/// If false, panics if sees JSX
	pub expect_jsx: bool,
	pub expect_cursors: bool,
}

impl Default for ToStringOptions {
	fn default() -> Self {
		ToStringOptions {
			pretty: true,
			include_types: false,
			include_decorators: false,
			include_comments: true,
			expect_jsx: false,
			trailing_semicolon: false,
			expect_cursors: false,
			indent_with: "\t".to_owned(),
		}
	}
}

impl ToStringOptions {
	pub fn minified() -> Self {
		ToStringOptions {
			pretty: false,
			include_comments: false,
			indent_with: "".to_owned(),
			..Default::default()
		}
	}

	/// With typescript type syntax
	pub fn typescript() -> Self {
		ToStringOptions { include_types: true, ..Default::default() }
	}

	/// Whether to include comment in source
	pub(crate) fn should_add_comment(&self) -> bool {
		self.pretty && self.include_comments
	}

	pub(crate) fn add_indent<T: source_map::ToString>(&self, indent: u8, buf: &mut T) {
		(0..indent).for_each(|_| buf.push_str(&self.indent_with))
	}

	pub(crate) fn add_gap<T: source_map::ToString>(&self, buf: &mut T) {
		if self.pretty {
			buf.push(' ');
		}
	}
}

/// Defines common methods that would exist on a AST part include position in source, creation from reader and
/// serializing to string from options.
///
/// TODO remove partial eq
pub trait ASTNode: Sized + Clone + PartialEq + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method from_reader
	fn from_string(
		script: String,
		options: ParseOptions,
		source: SourceId,
		offset: Option<u32>,
	) -> ParseResult<Self> {
		use source_map::LineStarts;

		// TODO take from argument
		let line_starts = LineStarts::new(script.as_str());

		lex_and_parse_script(line_starts, options, script, source, offset, Default::default())
	}

	/// Returns position of node as span AS IT WAS PARSED. May be Span::NULL if AST was doesn't match anything in source
	fn get_position(&self) -> &Span;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self>;

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	);

	/// Returns structure as valid string
	fn to_string(&self, options: &crate::ToStringOptions) -> String {
		let mut buf = String::new();
		self.to_string_from_buffer(&mut buf, options, 0);
		buf
	}
}

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: String,
	source: SourceId,
	offset: Option<u32>,
	cursors: Vec<(usize, CursorId<()>)>,
) -> Result<T, ParseError> {
	let (mut sender, mut reader) = tokenizer_lib::ParallelTokenQueue::new();
	let lex_options = options.get_lex_options();
	let length = script.len() as u32;
	let parsing_thread = std::thread::spawn(move || {
		let mut state = ParsingState {
			line_starts,
			source,
			length_of_source: length,
			constant_imports: Default::default(),
		};
		let res = T::from_reader(&mut reader, &mut state, &options);
		if res.is_ok() {
			reader.expect_next(TSXToken::EOS)?;
		}
		res
	});

	let lex_result = lexer::lex_script(&script, &mut sender, &lex_options, offset, cursors);
	if let Err((reason, pos)) = lex_result {
		return Err(ParseError::new(reason, pos));
	}
	drop(sender);
	parsing_thread.join().expect("Parsing panicked")
}

#[cfg(target_arch = "wasm32")]
#[doc(hidden)]
pub fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: String,
	source: SourceId,
	offset: Option<u32>,
	cursors: Vec<(usize, CursorId<()>)>,
) -> Result<T, ParseError> {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();
	let lex_result =
		lexer::lex_script(&script, &mut queue, &options.get_lex_options(), offset, cursors);

	if let Err((reason, pos)) = lex_result {
		return Err(ParseError::new(reason, pos));
	}

	let mut state = ParsingState {
		line_starts,
		length_of_source: script.len() as u32,
		source,
		constant_imports: Default::default(),
	};
	let res = T::from_reader(&mut queue, &mut state, &options);
	if res.is_ok() {
		queue.expect_next(TSXToken::EOS)?;
	}
	res
}

pub(crate) fn throw_unexpected_token<T>(
	reader: &mut impl TokenReader<TSXToken, source_map::Start>,
	expected: &[TSXToken],
) -> Result<T, ParseError> {
	throw_unexpected_token_with_token(reader.next().unwrap(), expected)
}

pub(crate) fn throw_unexpected_token_with_token<T>(
	token: Token<TSXToken, source_map::Start>,
	expected: &[TSXToken],
) -> Result<T, ParseError> {
	let position = token.get_span();
	Err(ParseError::new(ParseErrors::UnexpectedToken { expected, found: token.0 }, position))
}

#[derive(Debug)]
pub struct ParsingState {
	pub(crate) line_starts: source_map::LineStarts,
	pub(crate) source: source_map::SourceId,
	pub(crate) length_of_source: u32,
	/// TODO as multithreaded channel + record is dynamic exists
	pub(crate) constant_imports: Vec<String>,
}

/// A keyword
#[derive(Debug, Eq, Clone)]
pub struct Keyword<T: tokens::TSXKeywordNode>(pub T, pub Span);

// TODO name
#[cfg(feature = "serde-serialize")]
impl<T: tokens::TSXKeywordNode> serde::Serialize for Keyword<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		self.1.serialize(serializer)
	}
}

// Always true as T == T
impl<T: tokens::TSXKeywordNode + PartialEq> PartialEq for Keyword<T> {
	fn eq(&self, _: &Self) -> bool {
		true
	}
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: tokens::TSXKeywordNode> self_rust_tokenize::SelfRustTokenize for Keyword<T> {
	fn append_to_token_stream(
		&self,
		token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		let span_tokens = self_rust_tokenize::SelfRustTokenize::to_tokens(&self.1);
		token_stream.extend(self_rust_tokenize::quote!(Keyword(Default::default(), #span_tokens)));
	}
}

impl<T: tokens::TSXKeywordNode> Keyword<T>
where
	tokens::TSXKeyword: std::convert::From<T>,
{
	pub fn new(span: Span) -> Self {
		Keyword(T::default(), span)
	}

	pub(crate) fn optionally_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> Option<Self> {
		reader
			// There has to be a better way. Probably using constants
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::from(T::default())))
			.map(|token| Keyword::new(token.get_span()))
	}

	pub(crate) fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	) -> ParseResult<Self> {
		match reader.next() {
			Some(token) => {
				let keyword = TSXToken::Keyword(TSXKeyword::from(T::default()));
				let span = token.get_span();
				if token.0 == keyword {
					Ok(Self::new(span))
				} else {
					Err(ParseError::new(
						ParseErrors::UnexpectedToken { expected: &[keyword], found: token.0 },
						span,
					))
				}
			}
			None => Err(parse_lexing_error()),
		}
	}

	pub fn get_position(&self) -> &Span {
		&self.1
	}
}

impl<T: tokens::TSXKeywordNode> Visitable for Keyword<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		_options: &VisitSettings,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_keyword(&(self.0.into(), &self.1), data, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_options: &VisitSettings,
		_chain: &mut Annex<Chain>,
	) {
		// TODO should this have a implementation?
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum NumberSign {
	/// Also implies non negative/missing
	Positive,
	Negative,
}

impl NumberSign {
	pub fn apply<T: std::ops::Neg<Output = T>>(&self, x: T) -> T {
		match self {
			NumberSign::Positive => x,
			NumberSign::Negative => -x,
		}
	}
}

impl std::fmt::Display for NumberSign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if matches!(self, Self::Negative) {
			f.write_str("-")
		} else {
			Ok(())
		}
	}
}

/// TODO a mix between runtime numbers and source syntax based number
/// TODO hex cases lost in input :(
/// <https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-literals-numeric-literals>
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum NumberRepresentation {
	Infinity,
	NegativeInfinity,
	NaN,
	Hex(NumberSign, u64),
	Bin(NumberSign, u64),
	// Last one is whether it was specified with a leading zero (boo)
	Octal(NumberSign, u64, bool),
	Number {
		elided_zero_before_point: bool,
		trailing_point: bool,
		/// TODO could do as something other than f64
		internal: f64,
	},
	BigInt(NumberSign, String),
}

impl std::hash::Hash for NumberRepresentation {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
	}
}

impl From<NumberRepresentation> for f64 {
	fn from(this: NumberRepresentation) -> f64 {
		match this {
			NumberRepresentation::Infinity => f64::INFINITY,
			NumberRepresentation::NegativeInfinity => f64::NEG_INFINITY,
			NumberRepresentation::NaN => f64::NAN,
			NumberRepresentation::Number { internal, .. } => internal,
			NumberRepresentation::Hex(sign, nat)
			| NumberRepresentation::Bin(sign, nat)
			| NumberRepresentation::Octal(sign, nat, _) => sign.apply(nat as f64),
			NumberRepresentation::BigInt(..) => todo!(),
		}
	}
}

impl From<f64> for NumberRepresentation {
	fn from(value: f64) -> Self {
		if value == f64::INFINITY {
			Self::Infinity
		} else if value == f64::NEG_INFINITY {
			Self::NegativeInfinity
		} else if value.is_nan() {
			Self::NaN
		} else {
			Self::Number { internal: value, elided_zero_before_point: false, trailing_point: false }
		}
	}
}

impl FromStr for NumberRepresentation {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if s == "NaN" {
			return Ok(Self::NaN);
		}

		if s.contains('_') {
			return s.replace('_', "").parse();
		}

		let (sign, s) = if let Some(s) = s.strip_prefix('-') {
			(NumberSign::Negative, s)
		} else {
			(NumberSign::Positive, s)
		};

		let s = if s.contains('_') { Cow::Owned(s.replace('_', "")) } else { Cow::Borrowed(s) };

		if let Some(s) = s.strip_suffix('n') {
			Ok(NumberRepresentation::BigInt(sign, s.to_owned()))
		} else if let Some(s) = s.strip_prefix('0') {
			let next_char = s.chars().next();
			match next_char {
				Some('.') => {
					if s.len() == 2 {
						Ok(Self::Number {
							internal: 0f64,
							elided_zero_before_point: false,
							trailing_point: true,
						})
					} else {
						Ok(Self::Number {
							internal: sign.apply(s.parse().map_err(|_| s.to_owned())?),
							elided_zero_before_point: false,
							trailing_point: false,
						})
					}
				}
				Some('X' | 'x') => {
					let mut number = 0u64;
					for c in s[2..].as_bytes().iter() {
						number <<= 4; // 16=2^4
						match c {
							b'0'..=b'9' => {
								number += (c - b'0') as u64;
							}
							b'a'..=b'f' => {
								number += (c - b'a') as u64 + 10;
							}
							b'A'..=b'F' => {
								number += (c - b'A') as u64 + 10;
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Hex(sign, number))
				}
				Some('B' | 'b') => {
					let mut number = 0u64;
					for c in s[2..].as_bytes().iter() {
						number <<= 1;
						match c {
							b'0' | b'1' => {
								number += (c - b'0') as u64;
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Bin(sign, number))
				}
				// 'o' | 'O' but can also be missed
				Some(c) => {
					let uses_character = matches!(c, 'o' | 'O');
					let start = if uses_character { 2 } else { 1 };
					let mut number = 0u64;
					for c in s[start..].as_bytes().iter() {
						number <<= 3; // 8=2^3
						if matches!(c, b'0'..=b'7') {
							number += (c - b'0') as u64;
						} else {
							return Err(s.to_owned());
						}
					}
					Ok(Self::Octal(sign, number, !uses_character))
				}
				None => Ok(Self::Number {
					internal: 0f64,
					elided_zero_before_point: false,
					trailing_point: false,
				}),
			}
		} else if s.ends_with('.') {
			Ok(Self::Number {
				internal: sign.apply(s.strip_suffix('.').unwrap().parse().map_err(|_| s.clone())?),
				elided_zero_before_point: false,
				trailing_point: true,
			})
		} else if s.starts_with('.') {
			let value: f64 = s.strip_prefix('.').unwrap().parse().map_err(|_| s.clone())?;
			let digits = value.log10().floor() + 1f64;
			let result = value * (10f64.powf(-digits));
			Ok(Self::Number {
				internal: sign.apply(result),
				elided_zero_before_point: true,
				trailing_point: false,
			})
		} else if let Some((left, right)) = s.split_once(['e', 'E']) {
			let value: f64 = left.parse().map_err(|_| s.clone())?;
			let expo: i32 = right.parse().map_err(|_| s.clone())?;
			Ok(Self::Number {
				internal: sign.apply(value * 10f64.powi(expo)),
				elided_zero_before_point: false,
				trailing_point: false,
			})
		} else {
			Ok(Self::Number {
				internal: sign.apply(s.parse().map_err(|_| s.clone())?),
				elided_zero_before_point: false,
				trailing_point: false,
			})
		}
	}
}

impl std::fmt::Display for NumberRepresentation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.clone().as_js_string())
	}
}

impl PartialEq for NumberRepresentation {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			// TODO needs to do conversion
			(Self::Hex(l0, l1), Self::Hex(r0, r1)) => l0 == r0 && l1 == r1,
			(Self::Bin(l0, l1), Self::Bin(r0, r1)) => l0 == r0 && l1 == r1,
			(Self::Octal(l0, l1, _), Self::Octal(r0, r1, _)) => l0 == r0 && l1 == r1,
			(Self::Number { internal: l0, .. }, Self::Number { internal: r0, .. }) => l0 == r0,
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl Eq for NumberRepresentation {}

impl NumberRepresentation {
	pub fn negate(&self) -> Self {
		f64::from(self.clone()).neg().into()
	}

	pub fn as_js_string(self) -> String {
		match self {
			NumberRepresentation::Infinity => "Infinity".to_owned(),
			NumberRepresentation::NegativeInfinity => "-Infinity".to_owned(),
			NumberRepresentation::NaN => "NaN".to_owned(),
			NumberRepresentation::Hex(sign, value) => format!("{sign}{value:#x}"),
			NumberRepresentation::Bin(sign, value) => {
				format!("{sign}0b{value:#b}")
			}
			NumberRepresentation::Octal(sign, value, true) => {
				format!("{sign}0{value:o}")
			}
			NumberRepresentation::Octal(sign, value, false) => {
				format!("{sign}0o{value:o}")
			}
			NumberRepresentation::Number { internal, elided_zero_before_point, trailing_point } => {
				let mut start = internal.to_string();
				if elided_zero_before_point {
					start.remove(0);
				}
				if trailing_point {
					start.push('.')
				}
				start
			}
			NumberRepresentation::BigInt(s, value) => format!("{s}{value}n"),
		}
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

	pub fn is_async(&self) -> bool {
		matches!(self, Self::Regular { r#async: Some(_), .. })
	}

	pub fn is_generator(&self) -> bool {
		matches!(self, Self::Regular { generator: Some(_), .. })
	}

	fn is_some(&self) -> bool {
		!matches!(self, Self::Regular { r#async: None, generator: None })
	}

	// pub(crate) fn get_end(&self) -> source_map::End {
	// 	match self {
	// 		MethodHeader::Get(kw) => kw.1.get_end(),
	// 		MethodHeader::Set(kw) => kw.1.get_end(),
	// 		MethodHeader::Async(kw) => kw.1.get_end(),
	// 		MethodHeader::GeneratorStar(_, a) => a.get_end(),
	// 		#[cfg(feature = "extras")]
	// 		MethodHeader::Generator(_, a) => a.1.get_end(),
	// 	}
	// }
}

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition:
	Clone + std::fmt::Debug + Sync + Send + PartialEq + Eq + 'static
{
	type Name: Clone + std::fmt::Debug + Sync + Send + PartialEq + Eq + 'static;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::Name>;

	fn as_option_str(name: &Self::Name) -> Option<&str>;
	fn as_option_string_mut(name: &mut Self::Name) -> Option<&mut String>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StatementPosition;

impl ExpressionOrStatementPosition for StatementPosition {
	type Name = VariableIdentifier;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::Name> {
		VariableIdentifier::from_reader(reader, state, options)
	}

	fn as_option_str(name: &Self::Name) -> Option<&str> {
		if let VariableIdentifier::Standard(name, ..) = name {
			Some(name)
		} else {
			None
		}
	}

	fn as_option_string_mut(name: &mut Self::Name) -> Option<&mut String> {
		if let VariableIdentifier::Standard(name, ..) = name {
			Some(name)
		} else {
			None
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExpressionPosition;

impl ExpressionOrStatementPosition for ExpressionPosition {
	type Name = Option<VariableIdentifier>;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::Name> {
		if let Some(Token(TSXToken::OpenBrace, _)) | None = reader.peek() {
			Ok(None)
		} else {
			StatementPosition::from_reader(reader, state, options).map(Some)
		}
	}

	fn as_option_str(name: &Self::Name) -> Option<&str> {
		name.as_ref().and_then(StatementPosition::as_option_str)
	}

	fn as_option_string_mut(name: &mut Self::Name) -> Option<&mut String> {
		name.as_mut().and_then(StatementPosition::as_option_string_mut)
	}
}

/// Parses items surrounded in `{`, `[`, `(`, etc.
///
/// Supports trailing commas
pub(crate) fn parse_bracketed<T: ASTNode>(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
	start: Option<TSXToken>,
	end: TSXToken,
) -> ParseResult<(Vec<T>, TokenEnd)> {
	if let Some(start) = start {
		let _ = reader.expect_next(start)?;
	}
	let mut nodes: Vec<T> = Vec::new();
	loop {
		if let Some(token) = reader.conditional_next(|token| *token == end) {
			return Ok((nodes, token.get_end()));
		}
		nodes.push(T::from_reader(reader, state, options)?);
		match reader.next().ok_or_else(errors::parse_lexing_error)? {
			Token(TSXToken::Comma, _) => {}
			token => {
				if token.0 == end {
					let get_end = token.get_end();
					return Ok((nodes, get_end));
				}
				let position = token.get_span();
				return Err(ParseError::new(
					crate::ParseErrors::UnexpectedToken {
						expected: &[end, TSXToken::Comma],
						found: token.0,
					},
					position,
				));
			}
		}
	}
}

#[cfg(not(target_arch = "wasm32"))]
/// For demos and testing
pub fn script_to_tokens(source: String) -> impl Iterator<Item = (String, bool)> + 'static {
	let (mut sender, reader) = tokenizer_lib::ParallelTokenQueue::new();
	// TODO clone ...
	let input = source.clone();
	let _lexing_thread = std::thread::spawn(move || {
		let _lex_script =
			lexer::lex_script(&input, &mut sender, &Default::default(), None, Default::default());
		drop(sender);
	});

	receiver_to_tokens(reader, source)
}

#[cfg(target_arch = "wasm32")]
/// For demos and testing
pub fn script_to_tokens(source: String) -> impl Iterator<Item = (String, bool)> + 'static {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();

	let _lex_script =
		lexer::lex_script(&source, &mut queue, &Default::default(), None, Default::default());

	receiver_to_tokens(queue, source)
}

/// For testing and other features
fn receiver_to_tokens(
	mut receiver: impl TokenReader<TSXToken, TokenStart> + 'static,
	input: String,
) -> impl Iterator<Item = (String, bool)> + 'static {
	let mut last = 0u32;
	let mut last_section = None;
	std::iter::from_fn(move || {
		if last_section.is_some() {
			return last_section.take();
		}

		let token = receiver.next()?;
		if matches!(token.0, TSXToken::EOS) {
			return None;
		}
		let span = token.get_span();
		let start = span.start;
		let section =
			(input.get(std::ops::Range::from(span.clone())).unwrap_or("?").to_owned(), true);
		if last != start {
			last_section = Some(section);
			let token = input.get((last as usize)..(start as usize)).unwrap_or("?").to_owned();
			last = span.end;
			Some((token, false))
		} else {
			last = span.end;
			Some(section)
		}
	})
}

/// *to_strings* items surrounded in `{`, `[`, `(`, etc
///
/// TODO delimiter
pub(crate) fn to_string_bracketed<T: source_map::ToString, U: ASTNode>(
	nodes: &[U],
	brackets: (char, char),
	buf: &mut T,
	options: &crate::ToStringOptions,
	depth: u8,
) {
	buf.push(brackets.0);
	for (at_end, node) in nodes.iter().endiate() {
		node.to_string_from_buffer(buf, options, depth);
		if !at_end {
			buf.push(',');
			options.add_gap(buf);
		}
	}
	buf.push(brackets.1);
}

/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
pub(crate) fn expect_semi_colon(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	line_starts: &source_map::LineStarts,
	prev: u32,
) -> ParseResult<()> {
	if let Some(token) = reader.peek() {
		let Token(kind, start) = token;
		// eprintln!("{:?} {:?} {:?}", prev, next, line_starts);
		if let TSXToken::CloseBrace | TSXToken::EOS = kind {
			Ok(())
		} else if !matches!(kind, TSXToken::SemiColon)
			&& line_starts.byte_indexes_on_different_lines(prev as usize, start.0 as usize)
		{
			Ok(())
		} else {
			reader.expect_next(TSXToken::SemiColon).map(|_| ()).map_err(Into::into)
		}
	} else {
		Ok(())
	}
}

/// Re-exports or generator and general use
pub mod ast {
	pub use crate::{
		declarations::classes::*,
		declarations::*,
		expressions::*,
		extensions::jsx::*,
		functions::{
			FunctionBase, FunctionHeader, FunctionParameters, Parameter, ParameterData,
			SpreadParameter,
		},
		statements::*,
		Block, Decorated, Keyword, MethodHeader, NumberRepresentation, PropertyKey,
		StatementOrDeclaration, VariableField, VariableIdentifier, WithComment,
	};

	pub use source_map::{BaseSpan, SourceId};

	pub use self::assignments::{LHSOfAssignment, VariableOrPropertyAccess};
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) mod test_utils {
	#[macro_export]
	#[allow(clippy::crate_in_macro_def)]
	macro_rules! assert_matches_ast {
		($source:literal, $ast_pattern:pat) => {{
			let node = crate::ASTNode::from_string(
				$source.to_owned(),
				Default::default(),
				crate::SourceId::NULL,
				None,
			)
			.unwrap();
			// AST matchers are partial expressions
			let matches = ::match_deref::match_deref! {
				match &node {
					$ast_pattern => true,
					_ => false,
				}
			};

			if !matches {
				panic!("{:#?} did not match {}", node, stringify!($ast_pattern));
			}
		}};
	}

	#[macro_export]
	#[allow(clippy::crate_in_macro_def)]
	macro_rules! span {
		($start:pat, $end:pat) => {
			crate::Span { start: $start, end: $end, .. }
		};
	}
}

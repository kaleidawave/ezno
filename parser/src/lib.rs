#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default, clippy::too_many_lines)]
#![warn(clippy::must_use_candidate)]

mod block;
mod comments;
pub mod declarations;
mod errors;
pub mod expressions;
mod extensions;
pub mod functions;
pub mod generator_helpers;
mod lexer;
pub mod marker;
mod modules;
pub mod property_key;
pub mod statements;
mod tokens;
pub mod types;
mod variable_fields;
pub mod visiting;

pub use block::{Block, BlockLike, BlockLikeMut, BlockOrSingleStatement, StatementOrDeclaration};
pub use comments::WithComment;
pub use declarations::Declaration;
use functions::FunctionBody;
pub use marker::Marker;

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
pub use modules::Module;
pub use property_key::PropertyKey;
pub use source_map::{self, SourceId, Span};
pub use statements::Statement;
pub use tokens::{TSXKeyword, TSXToken};
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, TypeParameter},
};
pub use variable_fields::*;
pub(crate) use visiting::{
	Chain, ChainVariable, VisitOptions, Visitable, VisitorMutReceiver, VisitorReceiver,
};

use tokenizer_lib::{
	sized_tokens::{SizedToken, TokenEnd},
	Token, TokenReader,
};

pub(crate) use tokenizer_lib::sized_tokens::TokenStart;

use std::{borrow::Cow, str::FromStr};

use crate::errors::parse_lexing_error;

#[macro_use]
extern crate macro_rules_attribute;

attribute_alias! {
	// Warning: can produce errors when used with other macro attributes. Always put this attribute first
	// TODO #[derive(Debug, Clone)] and maybe some others
	#[apply(derive_ASTNode!)] =
		#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
		#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
		#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))];
}

/// What surrounds a string
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[apply(derive_ASTNode!)]
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

/// Options to customize parsing
#[allow(unused)]
#[derive(Copy, Clone)]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ParseOptions {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	/// allow type annotations
	pub type_annotations: bool,
	/// just definition file
	pub type_definition_module: bool,
	/// Allow custom characters in JSX attributes
	pub special_jsx_attributes: bool,
	/// Parses decorators on items
	pub decorators: bool,
	/// Skip **all** comments from the AST
	pub comments: Comments,
	/// See [`crate::extensions::is_expression::IsExpression`]
	pub is_expressions: bool,
	/// Allows functions to be prefixed with 'server'
	pub custom_function_headers: bool,
	/// TODO temp for seeing how channel performs
	pub buffer_size: usize,
	/// Has no effect on WASM. Increase for deeply nested AST structures
	pub stack_size: Option<usize>,
	/// Useful for LSP information
	pub record_keyword_positions: bool,
	/// For the generator
	pub interpolation_points: bool,
	/// Extra
	pub destructuring_type_annotation: bool,
	/// Extra
	pub extra_operators: bool,
	/// For formatting
	pub retain_blank_lines: bool,
	/// For LSP
	pub partial_syntax: bool,
}

impl ParseOptions {
	fn get_lex_options(&self) -> LexerOptions {
		LexerOptions {
			comments: self.comments,
			lex_jsx: self.jsx,
			allow_unsupported_characters_in_jsx_attribute_keys: self.special_jsx_attributes,
		}
	}

	#[must_use]
	pub fn all_features() -> Self {
		Self {
			jsx: true,
			type_annotations: true,
			type_definition_module: false,
			special_jsx_attributes: true,
			comments: Comments::All,
			decorators: true,
			custom_function_headers: true,
			is_expressions: true,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: true,
			// Only used in the AST-generator
			interpolation_points: false,
			partial_syntax: true,
			destructuring_type_annotation: true,
			extra_operators: true,
			retain_blank_lines: true,
		}
	}
}

// TODO unsure about some of these defaults, may change in future
impl Default for ParseOptions {
	fn default() -> Self {
		Self {
			jsx: true,
			type_annotations: true,
			type_definition_module: false,
			special_jsx_attributes: false,
			comments: Comments::All,
			decorators: true,
			custom_function_headers: false,
			is_expressions: false,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: false,
			interpolation_points: false,
			partial_syntax: false,
			destructuring_type_annotation: false,
			extra_operators: false,
			retain_blank_lines: false,
		}
	}
}

/// Settings for serializing `ASTNodes`
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ToStringOptions {
	/// Does not include whitespace minification
	pub pretty: bool,
	/// Blocks have trailing semicolons. Has no effect if pretty == false
	pub trailing_semicolon: bool,
	/// Single statements get put on the same line as their parent statement
	pub single_statement_on_new_line: bool,
	/// Include type annotations (and additional TypeScript) syntax
	pub include_type_annotations: bool,
	/// TODO unsure about this
	pub include_decorators: bool,
	pub comments: Comments,
	pub indent_with: String,
	/// If false, panics if sees JSX
	pub expect_jsx: bool,
	/// For partial AST, marker nodes may exist. This allows pretty printing on invalid source
	/// but should be `false` for builds
	///
	/// if `false` and a marker node is found, printing will panic
	pub expect_markers: bool,
	/// has no effect under !pretty
	pub max_line_length: u8,
}

impl Default for ToStringOptions {
	fn default() -> Self {
		ToStringOptions {
			pretty: true,
			include_type_annotations: false,
			single_statement_on_new_line: true,
			include_decorators: false,
			comments: Comments::All,
			expect_jsx: false,
			trailing_semicolon: false,
			expect_markers: false,
			indent_with: "\t".to_owned(),
			max_line_length: u8::MAX,
		}
	}
}

impl ToStringOptions {
	#[must_use]
	pub fn minified() -> Self {
		ToStringOptions {
			pretty: false,
			comments: Comments::None,
			indent_with: String::new(),
			..Default::default()
		}
	}

	/// With TypeScript type syntax
	#[must_use]
	pub fn typescript() -> Self {
		ToStringOptions { include_type_annotations: true, ..Default::default() }
	}

	/// Whether to include comment in source
	pub(crate) fn should_add_comment(&self, is_document_comment: bool) -> bool {
		matches!(self.comments, Comments::All)
			|| (matches!(self.comments, Comments::JustDocumentation) && is_document_comment)
	}

	pub(crate) fn add_indent<T: source_map::ToString>(&self, indent: u8, buf: &mut T) {
		if self.pretty {
			(0..indent).for_each(|_| buf.push_str(&self.indent_with));
		}
	}

	/// Adds whitespace **conditionally** (based on pretty setting)
	pub(crate) fn push_gap_optionally<T: source_map::ToString>(&self, buf: &mut T) {
		if self.pretty {
			buf.push(' ');
		}
	}

	pub(crate) fn enforce_limit_length_limit(&self) -> bool {
		self.pretty && self.max_line_length != u8::MAX
	}
}

#[derive(Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub enum Comments {
	#[default]
	All,
	/// Only multiline comments starting with `/**`
	JustDocumentation,
	None,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalToStringInformation {
	under: SourceId,
	depth: u8,
	should_try_pretty_print: bool,
}

impl LocalToStringInformation {
	#[must_use]
	pub fn new_under(under: SourceId) -> Self {
		Self { under, depth: 0, should_try_pretty_print: true }
	}

	pub(crate) fn next_level(self) -> Self {
		Self {
			under: self.under,
			depth: self.depth + 1,
			should_try_pretty_print: self.should_try_pretty_print,
		}
	}

	/// For printing source maps after bundling
	pub(crate) fn change_source(self, new: SourceId) -> Self {
		Self {
			under: new,
			depth: self.depth,
			should_try_pretty_print: self.should_try_pretty_print,
		}
	}

	/// Prevents recursion & other excess
	pub(crate) fn do_not_pretty_print(self) -> Self {
		Self { under: self.under, depth: self.depth, should_try_pretty_print: false }
	}
}

/// Defines common methods that would exist on a AST part include position in source, creation from reader and
/// serializing to string from options.
///
/// TODO remove partial eq
pub trait ASTNode: Sized + Clone + PartialEq + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method `from_reader`
	fn from_string(script: String, options: ParseOptions) -> ParseResult<Self> {
		Self::from_string_with_options(script, options, None).map(|(ast, _)| ast)
	}

	fn from_string_with_options(
		script: String,
		options: ParseOptions,
		offset: Option<u32>,
	) -> ParseResult<(Self, ParsingState)> {
		let line_starts = source_map::LineStarts::new(script.as_str());
		lex_and_parse_script(line_starts, options, &script, offset)
	}

	/// Returns position of node as span AS IT WAS PARSED. May be `Span::NULL` if AST was doesn't match anything in source
	fn get_position(&self) -> Span;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> ParseResult<Self>;

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	);

	/// Returns structure as valid string
	fn to_string(&self, options: &crate::ToStringOptions) -> String {
		let mut buf = source_map::StringWithOptionalSourceMap::new(false);
		let local = LocalToStringInformation::new_under(source_map::Nullable::NULL);
		self.to_string_from_buffer(&mut buf, options, local);
		buf.source
	}
}

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: &str,
	offset: Option<u32>,
) -> ParseResult<(T, ParsingState)> {
	let (mut sender, mut reader) =
		tokenizer_lib::ParallelTokenQueue::new_with_buffer_size(options.buffer_size);
	let lex_options = options.get_lex_options();

	#[allow(clippy::cast_possible_truncation)]
	let length_of_source = script.len() as u32;

	let mut thread = std::thread::Builder::new().name("AST parsing".into());
	if let Some(stack_size) = options.stack_size {
		thread = thread.stack_size(stack_size);
	}

	let parsing_thread = thread
		.spawn(move || {
			let mut state = ParsingState {
				line_starts,
				length_of_source,
				constant_imports: Default::default(),
				keyword_positions: options
					.record_keyword_positions
					.then_some(KeywordPositions::new()),
				partial_points: Default::default(),
			};
			let res = T::from_reader(&mut reader, &mut state, &options);
			if res.is_ok() {
				reader.expect_next(TSXToken::EOS)?;
			}
			res.map(|res| (res, state))
		})
		.unwrap();

	let lex_result = lexer::lex_script(script, &mut sender, &lex_options, offset);
	if let Err((reason, pos)) = lex_result {
		return Err(ParseError::new(reason, pos));
	}
	drop(sender);
	parsing_thread.join().expect("Parsing panicked")
}

// WASM has no threads, so this is a sequential version
#[cfg(target_arch = "wasm32")]
#[doc(hidden)]
pub fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: &str,
	offset: Option<u32>,
) -> ParseResult<(T, ParsingState)> {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();
	let lex_result = lexer::lex_script(script, &mut queue, &options.get_lex_options(), offset);

	if let Err((reason, pos)) = lex_result {
		return Err(ParseError::new(reason, pos));
	}

	let mut state = ParsingState {
		line_starts,
		length_of_source: script.len() as u32,
		constant_imports: Default::default(),
		keyword_positions: options.record_keyword_positions.then_some(KeywordPositions::new()),
		partial_points: Default::default(),
	};
	let res = T::from_reader(&mut queue, &mut state, &options);
	if res.is_ok() {
		queue.expect_next(TSXToken::EOS)?;
	}
	res.map(|res| (res, state))
}

pub(crate) fn throw_unexpected_token<T>(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	expected: &[TSXToken],
) -> ParseResult<T> {
	throw_unexpected_token_with_token(reader.next().unwrap(), expected)
}

pub(crate) fn throw_unexpected_token_with_token<T>(
	token: Token<TSXToken, TokenStart>,
	expected: &[TSXToken],
) -> ParseResult<T> {
	let position = token.get_span();
	Err(ParseError::new(ParseErrors::UnexpectedToken { expected, found: token.0 }, position))
}

#[derive(Debug)]
pub struct ParsingState {
	pub(crate) line_starts: source_map::LineStarts,
	pub(crate) length_of_source: u32,
	/// TODO as multithreaded channel + record is dynamic exists
	pub(crate) constant_imports: Vec<String>,
	pub keyword_positions: Option<KeywordPositions>,
	pub partial_points: Vec<TokenStart>,
}

impl ParsingState {
	pub(crate) fn expect_keyword(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> crate::ParseResult<TokenStart> {
		let start = reader.expect_next(TSXToken::Keyword(kw))?;
		self.append_keyword_at_pos(start.0, kw);
		Ok(start)
	}

	pub(crate) fn optionally_expect_keyword(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> Option<Span> {
		if let Some(Token(t, start)) = reader.conditional_next(|t| *t == TSXToken::Keyword(kw)) {
			self.append_keyword_at_pos(start.0, kw);
			Some(start.with_length(t.length() as usize))
		} else {
			None
		}
	}

	pub(crate) fn expect_keyword_get_full_span(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> crate::ParseResult<Span> {
		let start = reader.expect_next(TSXToken::Keyword(kw))?;
		self.append_keyword_at_pos(start.0, kw);
		Ok(start.with_length(kw.length() as usize))
	}

	fn append_keyword_at_pos(&mut self, start: u32, kw: TSXKeyword) {
		if let Some(ref mut keyword_positions) = self.keyword_positions {
			keyword_positions.0.push((start, kw));
		}
	}

	fn new_partial_point_marker<T>(&mut self, at: source_map::Start) -> Marker<T> {
		let id = self.partial_points.len();
		self.partial_points.push(at);
		Marker(u8::try_from(id).expect("more than 256 markers"), Default::default())
	}
}

/// As parsing is forwards, this is ordered
#[derive(Debug)]
pub struct KeywordPositions(Vec<(u32, TSXKeyword)>);

impl KeywordPositions {
	#[must_use]
	#[allow(clippy::cast_possible_truncation)]
	pub fn try_get_keyword_at_position(&self, pos: u32) -> Option<TSXKeyword> {
		// binary search
		let mut l: u32 = 0;
		let mut r: u32 = self.0.len() as u32 - 1u32;
		while l <= r {
			let m = (l + r) >> 1;
			let (kw_pos, kw) = self.0[m as usize];
			if kw_pos <= pos && pos < (kw_pos + kw.length()) {
				return Some(kw);
			} else if pos > kw_pos {
				l = m + 1;
			} else if pos < kw_pos {
				r = m - 1;
			}
		}
		None
	}

	fn new() -> Self {
		Self(Default::default())
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[apply(derive_ASTNode)]
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

impl std::ops::Neg for NumberSign {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			NumberSign::Positive => NumberSign::Negative,
			NumberSign::Negative => NumberSign::Positive,
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

/// Some of these can't be parsed, but are there to make so that a number expression can be generated from a f64
///
/// <https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-literals-numeric-literals>
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum NumberRepresentation {
	Infinity,
	NegativeInfinity,
	NaN,
	Hex { sign: NumberSign, value: u64 },
	Bin { sign: NumberSign, value: u64 },
	Octal { sign: NumberSign, value: u64 },
	Number(f64),
	Exponential { sign: NumberSign, value: f64, exponent: i32 },
	BigInt(NumberSign, String),
}

impl std::hash::Hash for NumberRepresentation {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
	}
}

impl TryFrom<NumberRepresentation> for f64 {
	// BigInt!!
	type Error = ();

	fn try_from(this: NumberRepresentation) -> Result<Self, Self::Error> {
		match this {
			NumberRepresentation::Infinity => Ok(f64::INFINITY),
			NumberRepresentation::NegativeInfinity => Ok(f64::NEG_INFINITY),
			NumberRepresentation::NaN => Ok(f64::NAN),
			NumberRepresentation::Number(value) => Ok(value),
			NumberRepresentation::Hex { sign, value, .. }
			| NumberRepresentation::Bin { sign, value, .. }
			| NumberRepresentation::Octal { sign, value, .. } => {
				// TODO `value as f64` can lose information? If so should return f64::INFINITY
				Ok(sign.apply(value as f64))
			}
			NumberRepresentation::Exponential { sign, value, exponent } => {
				Ok(sign.apply(value * 10f64.powi(exponent)))
			}
			NumberRepresentation::BigInt(..) => Err(()),
		}
	}
}

// For code generation
impl From<f64> for NumberRepresentation {
	fn from(value: f64) -> Self {
		if value == f64::INFINITY {
			Self::Infinity
		} else if value == f64::NEG_INFINITY {
			Self::NegativeInfinity
		} else if value.is_nan() {
			Self::NaN
		} else {
			Self::Number(value)
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
					if s.len() == 1 {
						Ok(Self::Number(0f64))
					} else {
						Ok(Self::Number(sign.apply(s.parse().map_err(|_| s.to_owned())?)))
					}
				}
				Some('X' | 'x') => {
					let mut value = 0u64;
					for c in s[1..].as_bytes() {
						value <<= 4; // 16=2^4
						match c {
							b'0'..=b'9' => {
								value += u64::from(c - b'0');
							}
							b'a'..=b'f' => {
								value += u64::from(c - b'a') + 10;
							}
							b'A'..=b'F' => {
								value += u64::from(c - b'A') + 10;
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Hex { sign, value })
				}
				Some('B' | 'b') => {
					let mut value = 0u64;
					for c in s[1..].as_bytes() {
						value <<= 1;
						match c {
							b'0' | b'1' => {
								value += u64::from(c - b'0');
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Bin { sign, value })
				}
				Some('e' | 'E') => {
					// Lol
					let exponent: i32 = s[1..].parse().map_err(|_| s.to_owned())?;
					Ok(Self::Exponential { sign, value: 0f64, exponent })
				}
				// 'o' | 'O' but can also be missed
				Some(c) => {
					let uses_character = matches!(c, 'o' | 'O');

					if !uses_character && s.contains(['8', '9', '.']) {
						return Ok(Self::Number(sign.apply(s.parse().map_err(|_| s.to_owned())?)));
					}

					// If it uses the the character then skip one, else skip zero
					let start: usize = uses_character.into();

					let mut value = 0u64;
					for c in s[start..].as_bytes() {
						value <<= 3; // 8=2^3
						if matches!(c, b'0'..=b'7') {
							value += u64::from(c - b'0');
						} else {
							return Err(s.to_owned());
						}
					}
					Ok(Self::Octal { sign, value })
				}
				None => Ok(Self::Number(0f64)),
			}
		} else if s.starts_with('.') {
			let value: f64 = format!("0{s}").parse().map_err(|_| s.clone())?;
			Ok(Self::Number(sign.apply(value)))
		} else if let Some(s) = s.strip_suffix('.') {
			Ok(Self::Number(sign.apply(s.parse::<f64>().map_err(|_| s)?)))
		} else if let Some((left, right)) = s.split_once(['e', 'E']) {
			let value = left.parse::<f64>().map_err(|_| s.clone())?;
			if let Ok(exponent) = right.parse::<i32>() {
				Ok(Self::Exponential { sign, value, exponent })
			} else if right.starts_with('-') || value == 0f64 {
				// lol
				Ok(Self::Number(0f64))
			} else {
				Ok(Self::Infinity)
			}
		} else {
			Ok(Self::Number(sign.apply(s.parse::<f64>().map_err(|_| s.clone())?)))
		}
	}
}

impl std::fmt::Display for NumberRepresentation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.clone().as_js_string())
	}
}

// TODO not great
impl PartialEq for NumberRepresentation {
	fn eq(&self, other: &Self) -> bool {
		if let (Ok(a), Ok(b)) = (f64::try_from(self.clone()), f64::try_from(other.clone())) {
			a == b
		} else {
			// TODO ...
			false
		}
	}
}

impl Eq for NumberRepresentation {}

impl std::ops::Neg for NumberRepresentation {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			NumberRepresentation::Infinity => NumberRepresentation::NegativeInfinity,
			NumberRepresentation::NegativeInfinity => NumberRepresentation::Infinity,
			NumberRepresentation::NaN => NumberRepresentation::NaN,
			NumberRepresentation::Hex { sign, value } => {
				NumberRepresentation::Hex { sign: sign.neg(), value }
			}
			NumberRepresentation::Bin { sign, value } => {
				NumberRepresentation::Bin { sign: sign.neg(), value }
			}
			NumberRepresentation::Octal { sign, value } => {
				NumberRepresentation::Octal { sign: sign.neg(), value }
			}
			NumberRepresentation::Number(n) => NumberRepresentation::Number(n.neg()),
			NumberRepresentation::Exponential { sign, value, exponent } => {
				NumberRepresentation::Exponential { sign: sign.neg(), value, exponent }
			}
			NumberRepresentation::BigInt(sign, value) => {
				NumberRepresentation::BigInt(sign.neg(), value)
			}
		}
	}
}

impl NumberRepresentation {
	#[must_use]
	pub fn as_js_string(self) -> String {
		match self {
			NumberRepresentation::Infinity => "Infinity".to_owned(),
			NumberRepresentation::NegativeInfinity => "-Infinity".to_owned(),
			NumberRepresentation::NaN => "NaN".to_owned(),
			NumberRepresentation::Hex { sign, value, .. } => {
				format!("{sign}0x{value:x}")
			}
			NumberRepresentation::Bin { sign, value, .. } => {
				format!("{sign}0b{value:b}")
			}
			NumberRepresentation::Octal { sign, value } => {
				format!("{sign}0o{value:o}")
			}
			NumberRepresentation::Number(value) => value.to_string(),
			NumberRepresentation::Exponential { sign, value, exponent } => {
				format!("{sign}{value}e{exponent}")
			}
			NumberRepresentation::BigInt(s, value) => format!("{s}{value}n"),
		}
	}
}

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition:
	Clone + std::fmt::Debug + Sync + Send + PartialEq + 'static
{
	type FunctionBody: ASTNode;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self>;

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier>;

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier>;

	fn as_option_str(&self) -> Option<&str> {
		if let Some(identifier) = self.as_option_variable_identifier() {
			identifier.as_option_str()
		} else {
			None
		}
	}

	fn has_function_body(body: &Self::FunctionBody) -> bool;

	fn is_declare(&self) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub struct StatementPosition {
	pub identifier: VariableIdentifier,
	pub declare: bool,
}

impl ExpressionOrStatementPosition for StatementPosition {
	type FunctionBody = FunctionBody;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		VariableIdentifier::from_reader(reader, state, options)
			.map(|identifier| Self { identifier, declare: false })
	}

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier> {
		Some(&self.identifier)
	}

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier> {
		Some(&mut self.identifier)
	}

	fn has_function_body(body: &Self::FunctionBody) -> bool {
		body.0.is_some()
	}

	fn is_declare(&self) -> bool {
		self.declare
	}
}

#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub struct ExpressionPosition(pub Option<VariableIdentifier>);

impl ExpressionOrStatementPosition for ExpressionPosition {
	type FunctionBody = Block;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(
			TSXToken::OpenBrace
			| TSXToken::OpenParentheses
			| TSXToken::Keyword(TSXKeyword::Extends),
			_,
		))
		| None = reader.peek()
		{
			Ok(Self(None))
		} else {
			Ok(Self(Some(VariableIdentifier::from_reader(reader, state, options)?)))
		}
	}

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier> {
		self.0.as_ref()
	}

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier> {
		self.0.as_mut()
	}

	fn has_function_body(_: &Self::FunctionBody) -> bool {
		true
	}

	fn is_declare(&self) -> bool {
		false
	}
}

pub trait ListItem: Sized {
	type LAST;
	const LAST_PREFIX: Option<TSXToken> = None;
	const EMPTY: Option<Self> = None;

	#[allow(unused)]
	fn parse_last_item(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self::LAST> {
		unreachable!("ListItem::LAST != ASTNode")
	}
}

/// Parses items surrounded in `{`, `[`, `(`, etc.
///
/// Supports trailing commas. But **does not create** *empty* like items afterwards
pub(crate) fn parse_bracketed<T: ASTNode + ListItem>(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
	start: Option<TSXToken>,
	end: TSXToken,
) -> ParseResult<(Vec<T>, Option<T::LAST>, TokenEnd)> {
	if let Some(start) = start {
		let _ = reader.expect_next(start)?;
	}
	let mut nodes: Vec<T> = Vec::new();
	loop {
		if let Some(empty) = T::EMPTY {
			let Token(next, _) = reader.peek().ok_or_else(parse_lexing_error)?;
			if matches!(next, TSXToken::Comma) || *next == end {
				if matches!(next, TSXToken::Comma) || (*next == end && !nodes.is_empty()) {
					nodes.push(empty);
				}
				let Token(token, s) = reader.next().unwrap();
				if token == end {
					return Ok((nodes, None, s.get_end_after(token.length() as usize)));
				}
				continue;
			}
		} else if let Some(token) = reader.conditional_next(|token| *token == end) {
			return Ok((nodes, None, token.get_end()));
		}

		if T::LAST_PREFIX.is_some_and(|l| reader.peek().is_some_and(|Token(token, _)| *token == l))
		{
			let last = T::parse_last_item(reader, state, options)?;
			let len = end.length() as usize;
			let end = reader.expect_next(end)?.get_end_after(len);
			return Ok((nodes, Some(last), end));
		}

		let node = T::from_reader(reader, state, options)?;
		nodes.push(node);

		match reader.next().ok_or_else(errors::parse_lexing_error)? {
			Token(TSXToken::Comma, _) => {}
			token => {
				if token.0 == end {
					let get_end = token.get_end();
					return Ok((nodes, None, get_end));
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
		let _lex_script = lexer::lex_script(&input, &mut sender, &Default::default(), None);
		drop(sender);
	});

	receiver_to_tokens(reader, source)
}

#[cfg(target_arch = "wasm32")]
/// For demos and testing
pub fn script_to_tokens(source: String) -> impl Iterator<Item = (String, bool)> + 'static {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();

	let _lex_script = lexer::lex_script(&source, &mut queue, &Default::default(), None);

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
		let section = (input.get(std::ops::Range::from(span)).unwrap_or("?").to_owned(), true);
		if last == start {
			last = span.end;
			Some(section)
		} else {
			last_section = Some(section);
			let token = input.get((last as usize)..(start as usize)).unwrap_or("?").to_owned();
			last = span.end;
			Some((token, false))
		}
	})
}

/// *`to_strings`* items surrounded in `{`, `[`, `(`, etc. Defaults to `,` as delimiter
pub(crate) fn to_string_bracketed<T: source_map::ToString, U: ASTNode>(
	nodes: &[U],
	(left_bracket, right_bracket): (char, char),
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	const MAX_INLINE_OBJECT_LITERAL: u32 = 40;
	let large =
		are_nodes_over_length(nodes.iter(), options, local, Some(MAX_INLINE_OBJECT_LITERAL), true);

	buf.push(left_bracket);
	let inner_local = if large {
		local.next_level()
	} else {
		if left_bracket == '{' {
			options.push_gap_optionally(buf);
		}
		local
	};
	for (at_end, node) in nodes.iter().endiate() {
		if large {
			buf.push_new_line();
			options.add_indent(inner_local.depth, buf);
		}
		node.to_string_from_buffer(buf, options, inner_local);
		if !at_end {
			buf.push(',');
			options.push_gap_optionally(buf);
		}
	}
	if large {
		buf.push_new_line();
		options.add_indent(local.depth, buf);
	} else if left_bracket == '{' {
		options.push_gap_optionally(buf);
	}
	buf.push(right_bracket);
}

/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
///
/// Also returns the line difference
pub(crate) fn expect_semi_colon(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	line_starts: &source_map::LineStarts,
	statement_end: u32,
	record_new_lines: bool,
) -> ParseResult<usize> {
	if let Some(token) = reader.peek() {
		let Token(kind, start) = token;

		if let TSXToken::CloseBrace
		| TSXToken::EOS
		| TSXToken::Comment(..)
		| TSXToken::MultiLineComment(..) = kind
		{
			Ok(line_starts
				.byte_indexes_crosses_lines(statement_end as usize, start.0 as usize + 1)
				.saturating_sub(1))
		} else if let TSXToken::SemiColon = kind {
			let Token(_, semicolon_end) = reader.next().unwrap();
			let Token(kind, next) = reader.peek().unwrap();
			if record_new_lines {
				let byte_indexes_crosses_lines = line_starts
					.byte_indexes_crosses_lines(semicolon_end.0 as usize, next.0 as usize + 1);

				// TODO WIP
				if let TSXToken::EOS = kind {
					Ok(byte_indexes_crosses_lines)
				} else {
					Ok(byte_indexes_crosses_lines.saturating_sub(1))
				}
			} else {
				Ok(0)
			}
		} else {
			let line_difference = line_starts
				.byte_indexes_crosses_lines(statement_end as usize, start.0 as usize + 1);
			if line_difference == 0 {
				throw_unexpected_token(reader, &[TSXToken::SemiColon])
			} else {
				Ok(line_difference - 1)
			}
		}
	} else {
		Ok(0)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum VariableKeyword {
	Const,
	Let,
	Var,
}

impl VariableKeyword {
	#[must_use]
	pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
		matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let | TSXKeyword::Var))
	}

	pub(crate) fn from_reader(token: Token<TSXToken, crate::TokenStart>) -> ParseResult<Self> {
		match token {
			Token(TSXToken::Keyword(TSXKeyword::Const), _) => Ok(Self::Const),
			Token(TSXToken::Keyword(TSXKeyword::Let), _) => Ok(Self::Let),
			Token(TSXToken::Keyword(TSXKeyword::Var), _) => Ok(Self::Var),
			token => crate::throw_unexpected_token_with_token(
				token,
				&[
					TSXToken::Keyword(TSXKeyword::Const),
					TSXToken::Keyword(TSXKeyword::Let),
					TSXToken::Keyword(TSXKeyword::Var),
				],
			),
		}
	}

	#[must_use]
	pub fn as_str(&self) -> &str {
		match self {
			Self::Const => "const ",
			Self::Let => "let ",
			Self::Var => "var ",
		}
	}
}

/// TODO WIP!
///
/// Conditionally computes the node length
/// Does nothing under pretty == false or no max line length
pub fn are_nodes_over_length<'a, T: ASTNode>(
	nodes: impl ExactSizeIterator<Item = &'a T>,
	options: &ToStringOptions,
	local: crate::LocalToStringInformation,
	// None = 'no space'
	available_space: Option<u32>,
	// Whether just to consider the amount on the line or the entire object
	total: bool,
) -> bool {
	if options.enforce_limit_length_limit() && local.should_try_pretty_print {
		let room = available_space.map_or(options.max_line_length as usize, |s| s as usize);
		let mut buf = source_map::StringWithOptionalSourceMap {
			source: String::new(),
			source_map: None,
			quit_after: Some(room),
			// Temp fix for considering delimiters to nodes
			since_new_line: nodes.len().try_into().expect("4 billion nodes ?"),
		};

		for node in nodes {
			node.to_string_from_buffer(&mut buf, options, local);

			let length = if total { buf.source.len() } else { buf.since_new_line as usize };
			let is_over = length > room;
			if is_over {
				return is_over;
			}
		}
		false
	} else {
		false
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
			FunctionBase, FunctionBody, FunctionHeader, FunctionParameters, MethodHeader,
			Parameter, ParameterData, SpreadParameter,
		},
		statements::*,
		Block, Decorated, ExpressionPosition, NumberRepresentation, PropertyKey,
		StatementOrDeclaration, StatementPosition, VariableField, VariableIdentifier, WithComment,
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
			let node = crate::ASTNode::from_string($source.to_owned(), Default::default()).unwrap();
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

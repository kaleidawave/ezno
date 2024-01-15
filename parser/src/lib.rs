#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default, clippy::too_many_lines)]

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
pub use tokens::{TSXKeyword, TSXToken};
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, GenericTypeConstraint, TypeDeclaration},
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

/// Options to customize parsing
#[allow(unused)]
#[derive(Copy, Clone)]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
pub struct ParseOptions {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	/// Allow custom characters in JSX attributes
	pub special_jsx_attributes: bool,
	/// Parses decorators on items
	pub decorators: bool,
	/// Skip **all** comments from the AST
	pub comments: Comments,
	/// See [crate::extensions::is_expression::IsExpression]
	pub is_expressions: bool,
	/// Allows functions to be prefixed with 'server'
	pub custom_function_headers: bool,
	/// For LSP allows incomplete AST for completions. TODO tidy up
	pub slots: bool,
	/// TODO temp for seeing how channel performs
	pub buffer_size: usize,
	/// TODO temp for seeing how channel performs
	///
	/// Has no effect on WASM
	pub stack_size: Option<usize>,
	/// Useful for LSP information
	pub record_keyword_positions: bool,
	/// For the generator
	pub interpolation_points: bool,
	/// For LSP
	pub partial_syntax: bool,
}

impl ParseOptions {
	fn get_lex_options(&self) -> LexerOptions {
		LexerOptions {
			comments: self.comments,
			lex_jsx: self.jsx,
			allow_unsupported_characters_in_jsx_attribute_keys: self.special_jsx_attributes,
			interpolation_points: self.interpolation_points,
		}
	}

	#[must_use]
	pub fn all_features() -> Self {
		Self {
			jsx: true,
			special_jsx_attributes: true,
			comments: Comments::All,
			decorators: true,
			slots: true,
			custom_function_headers: true,
			is_expressions: true,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: true,
			// Only used in the AST-generator
			interpolation_points: false,
			partial_syntax: true,
		}
	}
}

// TODO unsure about some of these defaults, may change in future
impl Default for ParseOptions {
	fn default() -> Self {
		Self {
			jsx: true,
			special_jsx_attributes: false,
			comments: Comments::All,
			decorators: true,
			slots: false,
			custom_function_headers: false,
			is_expressions: false,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: false,
			interpolation_points: false,
			partial_syntax: false,
		}
	}
}

/// Settings for serializing `ASTNodes`
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
pub struct ToStringOptions {
	/// Does not include whitespace minification
	pub pretty: bool,
	/// Blocks have trailing semicolons. Has no effect if pretty == false
	pub trailing_semicolon: bool,
	/// Single statements get put on the same line as their parent statement
	pub single_statement_on_new_line: bool,
	/// Include type annotation syntax
	pub include_types: bool,
	/// TODO unsure about this
	pub include_decorators: bool,
	pub comments: Comments,
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
			single_statement_on_new_line: true,
			include_decorators: false,
			comments: Comments::All,
			expect_jsx: false,
			trailing_semicolon: false,
			expect_cursors: false,
			indent_with: "\t".to_owned(),
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

	/// With typescript type syntax
	#[must_use]
	pub fn typescript() -> Self {
		ToStringOptions { include_types: true, ..Default::default() }
	}

	/// Whether to include comment in source
	pub(crate) fn should_add_comment(&self, document_comment: bool) -> bool {
		matches!(self.comments, Comments::All)
			|| (matches!(self.comments, Comments::JustDocumentation) && document_comment)
	}

	pub(crate) fn add_indent<T: source_map::ToString>(&self, indent: u8, buf: &mut T) {
		(0..indent).for_each(|_| buf.push_str(&self.indent_with));
	}

	/// Adds whitespace **conditionally** (based on pretty setting)
	pub(crate) fn add_gap<T: source_map::ToString>(&self, buf: &mut T) {
		if self.pretty {
			buf.push(' ');
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Comments {
	All,
	/// Only multiline comments starting with `/**`
	JustDocumentation,
	None,
}

/// Defines common methods that would exist on a AST part include position in source, creation from reader and
/// serializing to string from options.
///
/// TODO remove partial eq
pub trait ASTNode: Sized + Clone + PartialEq + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method `from_reader`
	fn from_string(
		script: String,
		options: ParseOptions,
		source: SourceId,
		offset: Option<u32>,
	) -> ParseResult<Self> {
		use source_map::LineStarts;

		// TODO take from argument
		let line_starts = LineStarts::new(script.as_str());

		lex_and_parse_script(line_starts, options, &script, source, offset)
	}

	/// Returns position of node as span AS IT WAS PARSED. May be `Span::NULL` if AST was doesn't match anything in source
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
	script: &str,
	source: SourceId,
	offset: Option<u32>,
) -> Result<T, ParseError> {
	let (mut sender, mut reader) =
		tokenizer_lib::ParallelTokenQueue::new_with_buffer_size(options.buffer_size);
	let lex_options = options.get_lex_options();
	let length = script.len() as u32;
	let mut thread = std::thread::Builder::new().name("AST parsing".into());
	if let Some(stack_size) = options.stack_size {
		thread = thread.stack_size(stack_size);
	}

	let parsing_thread = thread
		.spawn(move || {
			let mut state = ParsingState {
				line_starts,
				source,
				length_of_source: length,
				constant_imports: Default::default(),
				keyword_positions: options
					.record_keyword_positions
					.then_some(KeywordPositions::new()),
			};
			let res = T::from_reader(&mut reader, &mut state, &options);
			if res.is_ok() {
				reader.expect_next(TSXToken::EOS)?;
			}
			res
		})
		.unwrap();

	let lex_result = lexer::lex_script(script, &mut sender, &lex_options, offset);
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
	script: &str,
	source: SourceId,
	offset: Option<u32>,
) -> Result<T, ParseError> {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();
	let lex_result = lexer::lex_script(script, &mut queue, &options.get_lex_options(), offset);

	if let Err((reason, pos)) = lex_result {
		return Err(ParseError::new(reason, pos));
	}

	let mut state = ParsingState {
		line_starts,
		length_of_source: script.len() as u32,
		source,
		constant_imports: Default::default(),
		keyword_positions: options.record_keyword_positions.then_some(KeywordPositions::new()),
	};
	let res = T::from_reader(&mut queue, &mut state, &options);
	if res.is_ok() {
		queue.expect_next(TSXToken::EOS)?;
	}
	res
}

pub(crate) fn throw_unexpected_token<T>(
	reader: &mut impl TokenReader<TSXToken, TokenStart>,
	expected: &[TSXToken],
) -> Result<T, ParseError> {
	throw_unexpected_token_with_token(reader.next().unwrap(), expected)
}

pub(crate) fn throw_unexpected_token_with_token<T>(
	token: Token<TSXToken, TokenStart>,
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
	pub(crate) keyword_positions: Option<KeywordPositions>,
}

impl ParsingState {
	pub(crate) fn new_keyword(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> crate::ParseResult<TokenStart> {
		let start = reader.expect_next(TSXToken::Keyword(kw))?;
		self.add_keyword_at_pos(start.0, kw);
		Ok(start)
	}

	pub(crate) fn new_optional_keyword(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> Option<Span> {
		if let Some(Token(t, start)) = reader.conditional_next(|t| *t == TSXToken::Keyword(kw)) {
			self.add_keyword_at_pos(start.0, kw);
			Some(start.with_length(t.length() as usize))
		} else {
			None
		}
	}

	pub(crate) fn new_keyword_full_span(
		&mut self,
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		kw: TSXKeyword,
	) -> crate::ParseResult<Span> {
		let start = reader.expect_next(TSXToken::Keyword(kw))?;
		self.add_keyword_at_pos(start.0, kw);
		Ok(start.with_length(kw.length() as usize))
	}

	fn add_keyword_at_pos(&mut self, start: u32, kw: TSXKeyword) {
		if let Some(ref mut keyword_positions) = self.keyword_positions {
			keyword_positions.0.push((start, kw));
		}
	}
}

/// As parsing is forwards, this is ordered
#[derive(Debug)]
pub struct KeywordPositions(Vec<(u32, TSXKeyword)>);

impl KeywordPositions {
	#[must_use]
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
///
/// Some of these can't be parsed, but are there to make is so that a number can be generated from just a f64
#[derive(Debug, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum NumberRepresentation {
	Infinity,
	NegativeInfinity,
	NaN,
	Hex {
		sign: NumberSign,
		identifier_uppercase: bool,
		value: u64,
	},
	Bin {
		sign: NumberSign,
		identifier_uppercase: bool,
		value: u64,
	},
	Octal {
		sign: NumberSign,
		/// None = leading 0 (boo 👎)
		identifier_uppercase: Option<bool>,
		value: u64,
	},
	Number {
		/// TODO could do as something other than f64
		value: f64,
		/// To preserve formatting
		before_point: u8,
		/// To preserve formatting
		after_point: Option<u8>,
	},
	Exponential {
		sign: NumberSign,
		value: f64,
		exponent: i32,
		identifier_uppercase: bool,
	},
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
			NumberRepresentation::Number { value: internal, .. } => Ok(internal),
			NumberRepresentation::Hex { sign, value, .. }
			| NumberRepresentation::Bin { sign, value, .. }
			| NumberRepresentation::Octal { sign, value, .. } => Ok(sign.apply(value as f64)),
			NumberRepresentation::Exponential {
				sign,
				value,
				exponent,
				identifier_uppercase: _,
			} => Ok(sign.apply(value * 10f64.powi(exponent))),
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
			Self::Number {
				value,
				// These values should be fine
				before_point: 0,
				after_point: None,
			}
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
					let after_point = Some(s.len() as u8 - 1);
					let before_point = 1;
					if s.len() == 1 {
						Ok(Self::Number { value: 0f64, before_point, after_point })
					} else {
						Ok(Self::Number {
							value: sign.apply(s.parse().map_err(|_| s.to_owned())?),
							before_point,
							after_point,
						})
					}
				}
				Some(c @ ('X' | 'x')) => {
					let identifier_uppercase = c.is_uppercase();
					let mut value = 0u64;
					for c in s[2..].as_bytes() {
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
					Ok(Self::Hex { sign, identifier_uppercase, value })
				}
				Some(c @ ('B' | 'b')) => {
					let identifier_uppercase = c.is_uppercase();
					let mut value = 0u64;
					for c in s[2..].as_bytes() {
						value <<= 1;
						match c {
							b'0' | b'1' => {
								value += u64::from(c - b'0');
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Bin { identifier_uppercase, sign, value })
				}
				Some(c @ ('e' | 'E')) => {
					let exponent: i32 = s[1..].parse().map_err(|_| s.to_owned())?;
					Ok(Self::Exponential {
						sign,
						value: 0f64,
						exponent,
						identifier_uppercase: c.is_uppercase(),
					})
				}
				// 'o' | 'O' but can also be missed
				Some(c) => {
					let uses_character = matches!(c, 'o' | 'O');

					if !uses_character && s.contains(['8', '9', '.']) {
						let (before_point, after_point) =
							s.split_once('.').map_or((s.len() as u8 + 1, None), |(l, r)| {
								(l.len() as u8 + 1, Some(r.len() as u8))
							});

						return Ok(Self::Number {
							value: sign.apply(s.parse().map_err(|_| s.to_owned())?),
							before_point,
							after_point,
						});
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
					Ok(Self::Octal {
						sign,
						value,
						identifier_uppercase: uses_character.then_some(c.is_uppercase()),
					})
				}
				None => Ok(Self::Number { value: 0f64, before_point: 1, after_point: None }),
			}
		} else if s.starts_with('.') {
			let value: f64 = format!("0{s}").parse().map_err(|_| s.clone())?;
			Ok(Self::Number {
				value: sign.apply(value),
				before_point: 0,
				after_point: Some(s.len() as u8 - 1),
			})
		} else if let Some(s) = s.strip_suffix('.') {
			Ok(Self::Number {
				value: sign.apply(s.parse().map_err(|_| s)?),
				before_point: s.len() as u8,
				after_point: Some(0),
			})
		} else if let Some((left, right)) = s.split_once('e') {
			let value: f64 = left.parse().map_err(|_| s.clone())?;
			let exponent: i32 = right.parse().map_err(|_| s.clone())?;
			Ok(Self::Exponential { sign, value, exponent, identifier_uppercase: false })
		} else if let Some((left, right)) = s.split_once('E') {
			let value: f64 = left.parse().map_err(|_| s.clone())?;
			let exponent: i32 = right.parse().map_err(|_| s.clone())?;
			Ok(Self::Exponential { sign, value, exponent, identifier_uppercase: true })
		} else {
			let (before_point, after_point) = s
				.split_once('.')
				.map_or((s.len() as u8, None), |(l, r)| (l.len() as u8, Some(r.len() as u8)));

			Ok(Self::Number {
				value: sign.apply(s.parse().map_err(|_| s.clone())?),
				before_point,
				after_point,
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
		if let (Ok(a), Ok(b)) = (f64::try_from(self.clone()), f64::try_from(other.clone())) {
			a == b
		} else {
			// TODO ...
			false
		}
	}
}

impl Eq for NumberRepresentation {}

impl NumberRepresentation {
	#[must_use]
	pub fn as_js_string(self) -> String {
		match self {
			NumberRepresentation::Infinity => "Infinity".to_owned(),
			NumberRepresentation::NegativeInfinity => "-Infinity".to_owned(),
			NumberRepresentation::NaN => "NaN".to_owned(),
			NumberRepresentation::Hex { sign, identifier_uppercase: true, value, .. } => {
				format!("{sign}0X{value:X}")
			}
			NumberRepresentation::Hex { sign, identifier_uppercase: false, value, .. } => {
				format!("{sign}0x{value:x}")
			}
			NumberRepresentation::Bin { sign, identifier_uppercase: true, value, .. } => {
				format!("{sign}0B{value}")
			}
			NumberRepresentation::Bin { sign, identifier_uppercase: false, value, .. } => {
				format!("{sign}0b{value}")
			}
			NumberRepresentation::Octal { identifier_uppercase: None, sign, value } => {
				format!("{sign}0{value:o}")
			}
			NumberRepresentation::Octal { identifier_uppercase: Some(true), sign, value } => {
				format!("{sign}0O{value:o}")
			}
			NumberRepresentation::Octal { identifier_uppercase: Some(false), sign, value } => {
				format!("{sign}0o{value:o}")
			}
			NumberRepresentation::Number { value, after_point, before_point } => {
				let is_negative = value.is_sign_negative();
				let mut buf = value.abs().to_string();

				// TODO only `options.preserve_formatting`
				let (bp, ap) = buf
					.split_once('.')
					.map_or((buf.len() as u8, None), |(l, r)| (l.len() as u8, Some(r.len() as u8)));

				// Remove leading zero
				if bp > before_point {
					let removed = buf.remove(0);
					debug_assert_eq!(removed, '0');
				}

				(bp..before_point).for_each(|_| buf.insert(0, '0'));
				if let Some(after_point) = after_point {
					if ap.is_none() {
						buf.push('.');
					}
					(ap.unwrap_or_default()..after_point).for_each(|_| buf.push('0'));
				}
				if is_negative {
					buf.insert(0, '-');
				}
				buf
			}
			NumberRepresentation::Exponential { sign, value, exponent, identifier_uppercase } => {
				if identifier_uppercase {
					format!("{sign}{value}E{exponent}")
				} else {
					format!("{sign}{value}e{exponent}")
				}
			}
			NumberRepresentation::BigInt(s, value) => format!("{s}{value}n"),
		}
	}
}

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition:
	Clone + std::fmt::Debug + Sync + Send + PartialEq + Eq + 'static
{
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self>;

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier>;

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier>;

	fn as_option_str(&self) -> Option<&str> {
		if let Some(VariableIdentifier::Standard(name, _)) = self.as_option_variable_identifier() {
			Some(name)
		} else {
			None
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct StatementPosition(VariableIdentifier);

impl ExpressionOrStatementPosition for StatementPosition {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		VariableIdentifier::from_reader(reader, state, options).map(Self)
	}

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier> {
		Some(&self.0)
	}

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier> {
		Some(&mut self.0)
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct ExpressionPosition(Option<VariableIdentifier>);

impl ExpressionOrStatementPosition for ExpressionPosition {
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
}

/// Parses items surrounded in `{`, `[`, `(`, etc.
///
/// Supports trailing commas. But **does not create** *empty* like items afterwards
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
		let _lex_script = lexer::lex_script(&input, &mut sender, &Default::default(), None);
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

/// *`to_strings`* items surrounded in `{`, `[`, `(`, etc
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
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

/// Re-exports or generator and general use
pub mod ast {
	pub use crate::{
		declarations::classes::*,
		declarations::*,
		expressions::*,
		extensions::jsx::*,
		functions::{
			FunctionBase, FunctionHeader, FunctionParameters, MethodHeader, Parameter,
			ParameterData, SpreadParameter,
		},
		statements::*,
		Block, Decorated, NumberRepresentation, PropertyKey, StatementOrDeclaration, VariableField,
		VariableIdentifier, WithComment,
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

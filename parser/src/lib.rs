#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default)]

mod block;
mod comments;
pub mod cursor;
pub mod declarations;
mod errors;
pub mod expressions;
mod extensions;
pub mod functions;
mod generator_helpers;
mod modules;
pub mod operators;
pub mod parameters;
pub mod property_key;
pub mod statements;
mod tokens;
pub mod types;
mod variable_fields;
mod visiting;

#[doc(hidden)]
pub mod lexer;

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
use temporary_annex::Annex;
pub use tokens::{tsx_keywords, TSXKeyword, TSXToken};
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, GenericTypeConstraint, TypeDeclaration},
};
pub use variable_fields::*;
pub use visiting::*;

use tokenizer_lib::{Token, TokenReader};

use std::{borrow::Cow, ops::Neg, str::FromStr};

/// The notation of a string
#[derive(PartialEq, Debug, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
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
#[derive(Clone)]
pub struct ParseOptions {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	pub special_jsx_attributes: bool,
	pub decorators: bool,
	pub generator_keyword: bool,
	pub include_comments: bool,

	pub server_blocks: bool,
	pub module_blocks: bool,
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
			server_blocks: false,
			module_blocks: false,
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
			indent_with: "    ".to_owned(),
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
/// serializing to string from settings.
///
/// TODO remove partial eq
pub trait ASTNode: Sized + Clone + PartialEq + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method from_reader
	fn from_string(
		source: String,
		settings: ParseOptions,
		source_id: SourceId,
		offset: Option<usize>,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<Self> {
		use source_map::LineStarts;

		// TODO take from argument
		let line_starts = LineStarts::new(source.as_str());

		lex_and_parse_script(line_starts, settings, source, source_id, offset, cursors)
	}

	/// Returns position of node as span AS IT WAS PARSED. May be none if AST was doesn't match anything in source
	fn get_position(&self) -> Cow<Span>;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self>;

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	);

	/// Returns structure as valid string
	fn to_string(&self, settings: &crate::ToStringOptions) -> String {
		let mut buf = String::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf
	}
}

#[cfg(not(target_arch = "wasm32"))]
fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: String,
	source: SourceId,
	offset: Option<usize>,
	cursors: Vec<(usize, CursorId<()>)>,
) -> Result<T, ParseError> {
	let (mut sender, mut reader) = tokenizer_lib::ParallelTokenQueue::new();
	let lex_options = options.get_lex_options();
	let parsing_thread = std::thread::spawn(move || {
		let mut state = ParsingState { line_starts };
		let res = T::from_reader(&mut reader, &mut state, &options);
		if res.is_ok() {
			reader.expect_next(TSXToken::EOS)?;
		}
		res
	});

	lexer::lex_script(&script, &mut sender, &lex_options, Some(source), offset, cursors)?;
	drop(sender);

	parsing_thread.join().expect("Parsing panicked")
}

#[cfg(target_arch = "wasm32")]
fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: String,
	source: SourceId,
	offset: Option<usize>,
	cursors: Vec<(usize, CursorId<()>)>,
) -> Result<T, ParseError> {
	let mut queue = tokenizer_lib::BufferedTokenQueue::new();
	lexer::lex_script(
		&script,
		&mut queue,
		&LexerOptions { lex_jsx: false, ..options.get_lex_options() },
		Some(source),
		offset,
		cursors,
	)?;

	let mut state = ParsingState { line_starts };
	let res = T::from_reader(&mut queue, &mut state, &options);
	if res.is_ok() {
		queue.expect_next(TSXToken::EOS)?;
	}
	res
}

#[derive(Debug)]
pub struct ParsingState {
	pub(crate) line_starts: source_map::LineStarts,
}

/// A keyword
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Keyword<T: tokens::TSXKeywordNode>(pub T, pub Span);

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

impl<T: tokens::TSXKeywordNode> Keyword<T> {
	pub fn new(span: Span) -> Self {
		Keyword(T::default(), span)
	}

	// TODO
	pub(crate) fn _from_reader() {
		todo!("keyword from reader")
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
		_settings: &VisitSettings,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_keyword(&(self.0.into(), &self.1), data, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_settings: &VisitSettings,
		_chain: &mut Annex<Chain>,
	) {
		// TODO should this have a implementation?
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
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

/// TODO BigInt
/// TODO a mix between runtime numbers and source syntax based number
/// <https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-literals-numeric-literals>
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum NumberStructure {
	Infinity,
	NegativeInfinity,
	NaN,
	Hex(NumberSign, u64),
	Bin(NumberSign, u64),
	Octal(NumberSign, u64),
	/// TODO could do as something other than f64
	Number(f64),
}

impl std::hash::Hash for NumberStructure {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
	}
}

impl From<NumberStructure> for f64 {
	fn from(this: NumberStructure) -> f64 {
		match this {
			NumberStructure::Infinity => f64::INFINITY,
			NumberStructure::NegativeInfinity => f64::NEG_INFINITY,
			NumberStructure::NaN => f64::NAN,
			NumberStructure::Number(value) => value,
			NumberStructure::Hex(sign, nat)
			| NumberStructure::Bin(sign, nat)
			| NumberStructure::Octal(sign, nat) => sign.apply(nat as f64),
		}
	}
}

impl From<f64> for NumberStructure {
	fn from(x: f64) -> Self {
		if x == f64::INFINITY {
			Self::Infinity
		} else if x == f64::NEG_INFINITY {
			Self::NegativeInfinity
		} else if x.is_nan() {
			Self::NaN
		} else {
			Self::Number(x)
		}
	}
}

impl FromStr for NumberStructure {
	type Err = String;

	// TODO separators
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if s == "NaN" {
			return Ok(Self::NaN);
		}
		let (sign, s) = if let Some(s) = s.strip_prefix('-') {
			(NumberSign::Negative, s)
		} else {
			(NumberSign::Positive, s)
		};
		if let Some(s) = s.strip_prefix('0') {
			let next_char = s.chars().next();
			match next_char {
				Some('.') => Ok(Self::Number(sign.apply(s.parse().map_err(|_| s.to_owned())?))),
				Some('X' | 'x') => {
					let mut number = 0u64;
					for c in s[2..].as_bytes().iter().rev() {
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
				Some('b' | 'B') => {
					let mut number = 0u64;
					for c in s[2..].as_bytes().iter().rev() {
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
					let start = if matches!(c, 'o' | 'O') { 2 } else { 1 };
					let mut number = 0u64;
					for c in s[start..].as_bytes().iter().rev() {
						number <<= 3; // 8=2^3
						if matches!(c, b'0'..=b'7') {
							number += (c - b'0') as u64;
						} else {
							return Err(s.to_owned());
						}
					}
					Ok(Self::Octal(sign, number))
				}
				None => Ok(Self::Number(0.)),
			}
		} else {
			Ok(Self::Number(sign.apply(s.parse().map_err(|_| s.to_owned())?)))
		}
	}
}

impl std::fmt::Display for NumberStructure {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_js_string())
	}
}

impl PartialEq for NumberStructure {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			// TODO needs to do conversion
			(Self::Hex(l0, l1), Self::Hex(r0, r1)) => l0 == r0 && l1 == r1,
			(Self::Bin(l0, l1), Self::Bin(r0, r1)) => l0 == r0 && l1 == r1,
			(Self::Octal(l0, l1), Self::Octal(r0, r1)) => l0 == r0 && l1 == r1,
			(Self::Number(l0), Self::Number(r0)) => l0 == r0,
			_ => core::mem::discriminant(self) == core::mem::discriminant(other),
		}
	}
}

impl Eq for NumberStructure {}

impl NumberStructure {
	pub fn negate(&self) -> Self {
		f64::from(*self).neg().into()
	}

	pub fn as_js_string(self) -> String {
		match self {
			NumberStructure::Infinity => "Infinity".to_owned(),
			NumberStructure::NegativeInfinity => "-Infinity".to_owned(),
			NumberStructure::NaN => "NaN".to_owned(),
			NumberStructure::Hex(sign, value) => format!("{sign}0x{value}"),
			NumberStructure::Bin(sign, value) => format!("{sign}0b{value}"),
			NumberStructure::Octal(sign, value) => format!("{sign}0o{value}"),
			NumberStructure::Number(number) => number.to_string(),
		}
	}
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum GetSetGeneratorOrNone {
	Get(Keyword<tsx_keywords::Get>),
	Set(Keyword<tsx_keywords::Set>),
	#[cfg(feature = "extras")]
	Generator(Keyword<tsx_keywords::Generator>),
	GeneratorStar(Span),
	#[default]
	None,
}

impl GetSetGeneratorOrNone {
	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(&self, buf: &mut T) {
		buf.push_str(match self {
			GetSetGeneratorOrNone::Get(_) => "get ",
			GetSetGeneratorOrNone::Set(_) => "set ",
			GetSetGeneratorOrNone::Generator(_) | GetSetGeneratorOrNone::GeneratorStar(_) => "*",
			GetSetGeneratorOrNone::None => "",
		})
	}

	pub(crate) fn from_reader(reader: &mut impl TokenReader<TSXToken, Span>) -> Self {
		match reader.peek() {
			Some(Token(TSXToken::Keyword(TSXKeyword::Get), _)) => {
				let Token(_, span) = reader.next().unwrap();
				GetSetGeneratorOrNone::Get(Keyword::new(span))
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::Set), _)) => {
				let Token(_, span) = reader.next().unwrap();
				GetSetGeneratorOrNone::Set(Keyword::new(span))
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::Generator), _)) => {
				let Token(_, span) = reader.next().unwrap();
				GetSetGeneratorOrNone::Generator(Keyword::new(span))
			}
			Some(Token(TSXToken::Multiply, _)) => {
				let Token(_, span) = reader.next().unwrap();
				GetSetGeneratorOrNone::GeneratorStar(span)
			}
			_ => GetSetGeneratorOrNone::None,
		}
	}

	pub(crate) fn get_position(&self) -> Option<Cow<Span>> {
		match self {
			GetSetGeneratorOrNone::Get(kw) => Some(Cow::Borrowed(&kw.1)),
			GetSetGeneratorOrNone::Set(kw) => Some(Cow::Borrowed(&kw.1)),
			GetSetGeneratorOrNone::Generator(kw) => Some(Cow::Borrowed(&kw.1)),
			GetSetGeneratorOrNone::GeneratorStar(span) => Some(Cow::Borrowed(span)),
			GetSetGeneratorOrNone::None => None,
		}
	}
}

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition:
	Clone + std::fmt::Debug + Sync + Send + PartialEq + Eq + 'static
{
	type Name: Clone + std::fmt::Debug + Sync + Send + PartialEq + Eq + 'static;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self::Name>;

	fn as_option_str(name: &Self::Name) -> Option<&str>;
	fn as_option_string_mut(name: &mut Self::Name) -> Option<&mut String>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StatementPosition;

impl ExpressionOrStatementPosition for StatementPosition {
	type Name = VariableIdentifier;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self::Name> {
		VariableIdentifier::from_reader(reader, state, settings)
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
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self::Name> {
		if let Some(Token(TSXToken::OpenBrace, _)) | None = reader.peek() {
			Ok(None)
		} else {
			StatementPosition::from_reader(reader, state, settings).map(Some)
		}
	}

	fn as_option_str(name: &Self::Name) -> Option<&str> {
		name.as_ref().and_then(StatementPosition::as_option_str)
	}

	fn as_option_string_mut(name: &mut Self::Name) -> Option<&mut String> {
		name.as_mut().and_then(StatementPosition::as_option_string_mut)
	}
}

/// Parses items surrounded in `{`, `[`, `(`, etc
pub(crate) fn parse_bracketed<T: ASTNode>(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseOptions,
	start: Option<TSXToken>,
	end: TSXToken,
) -> ParseResult<(Vec<T>, Span)> {
	if let Some(start) = start {
		let _ = reader.expect_next(start)?;
	}
	let mut nodes: Vec<T> = Vec::new();
	loop {
		if let Some(Token(_, pos)) = reader.conditional_next(|token| *token == end) {
			return Ok((nodes, pos));
		}
		nodes.push(T::from_reader(reader, state, settings)?);
		match reader.next().ok_or_else(errors::parse_lexing_error)? {
			Token(TSXToken::Comma, _) => {}
			Token(token, pos) if token == end => return Ok((nodes, pos)),
			Token(token, position) => {
				return Err(ParseError::new(
					crate::ParseErrors::UnexpectedToken {
						expected: &[end, TSXToken::Comma],
						found: token,
					},
					position,
				))
			}
		}
	}
}

/// *to_strings* items surrounded in `{`, `[`, `(`, etc
pub(crate) fn to_string_bracketed<T: source_map::ToString, U: ASTNode>(
	nodes: &[U],
	brackets: (char, char),
	buf: &mut T,
	settings: &crate::ToStringOptions,
	depth: u8,
) {
	buf.push(brackets.0);
	for (at_end, node) in nodes.iter().endiate() {
		node.to_string_from_buffer(buf, settings, depth);
		if !at_end {
			buf.push(',');
			settings.add_gap(buf);
		}
	}
	buf.push(brackets.1);
}

/// Part of [ASI](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#automatic_semicolon_insertion)
pub(crate) fn expect_semi_colon(
	reader: &mut impl TokenReader<TSXToken, Span>,
	line_starts: &source_map::LineStarts,
	prev: u32,
) -> ParseResult<()> {
	if let Some(token) = reader.peek() {
		let Token(kind, Span { start: next, .. }) = token;
		// eprintln!("{:?} {:?} {:?}", prev, next, line_starts);
		if let TSXToken::CloseBrace | TSXToken::EOS = kind {
			Ok(())
		} else if !matches!(kind, TSXToken::SemiColon)
			&& line_starts.byte_indexes_on_different_lines(prev as usize, *next as usize)
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
		declarations::*, expressions::*, extensions::jsx::*, statements::*, Keyword,
		NumberStructure, StatementOrDeclaration, VariableField, VariableIdentifier, WithComment,
	};

	pub use self::assignments::{LHSOfAssignment, VariableOrPropertyAccess};
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) mod test_utils {
	#[macro_export]
	macro_rules! assert_matches_ast {
		($source:literal, $ast_pattern:pat) => {{
			let node = crate::ASTNode::from_string(
				$source.to_owned(),
				Default::default(),
				crate::SourceId::NULL,
				None,
				Vec::new(),
			)
			.unwrap();
			assert!(
				::match_deref::match_deref! {
					match &node {
						$ast_pattern => true,
						_ => false,
					}
				},
				"{:#?} did not match {}",
				node,
				stringify!($pattern)
			)
		}};
	}

	#[macro_export]
	macro_rules! span {
		($start:pat, $end:pat) => {
			crate::Span { start: $start, end: $end, .. }
		};
	}
}

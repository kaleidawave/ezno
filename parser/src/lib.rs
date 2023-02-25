#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default)]

mod block;
mod comments;
pub mod cursor;
mod errors;
pub mod expressions;
mod extensions;
pub mod extractor;
pub mod functions;
mod generator_helpers;
mod modules;
pub mod operators;
pub mod parameters;
mod property_key;
pub mod statements;
mod tokens;
mod types;
mod variable_fields;
mod visiting;

#[doc(hidden)]
pub mod lexer;

pub use block::{Block, BlockId, BlockLike, BlockLikeMut, BlockOrSingleStatement};
pub use comments::WithComment;
pub use cursor::{CursorId, EmptyCursorId};
pub use errors::{ParseError, ParseErrors, ParseResult};
pub use expressions::{Expression, PropertyReference};
pub use extensions::{
	decorators::{Decorated, Decorator},
	is_expression,
	jsx::*,
};
use extractor::ExtractedFunctions;
pub use extractor::UniversalFunctionId;
pub use functions::{FunctionBase, FunctionBased, FunctionHeader, FunctionId};
pub use generator_helpers::IntoAST;
use iterator_endiate::EndiateIteratorExt;
pub use lexer::{lex_source, LexSettings};
pub use modules::{FromFileError, Module, TypeDefinitionModule, TypeDefinitionModuleStatement};
pub use parameters::{
	FunctionParameters, OptionalOrWithDefaultValueParameter, Parameter, SpreadParameter,
};
pub use property_key::{PropertyId, PropertyKey};
pub use source_map::{SourceId, Span};
pub(crate) use statements::parse_interface_members;
use statements::StatementFunctionBase;
pub use statements::{InterfaceMember, Statement};
use temporary_annex::Annex;
pub use tokens::{tsx_keywords, TSXKeyword, TSXToken};
pub use types::{
	type_declarations,
	type_declarations::{GenericTypeConstraint, TypeDeclaration},
	type_references,
	type_references::TypeReference,
	TypeId,
};
pub use variable_fields::*;
pub use visiting::*;

use tokenizer_lib::{Token, TokenReader};

use std::{borrow::Cow, collections::HashMap, ops::Neg, str::FromStr};

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
pub struct ParseSettings {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	pub decorators: bool,
	pub generator_keyword: bool,

	pub server_blocks: bool,
	pub module_blocks: bool,
	/// For LSP allows incomplete AST for completions. TODO tidy up
	pub slots: bool,
}

// TODO not sure about some of these defaults, may change in future
impl Default for ParseSettings {
	fn default() -> Self {
		Self {
			jsx: true,
			decorators: true,
			slots: false,
			generator_keyword: true,
			server_blocks: false,
			module_blocks: false,
		}
	}
}

#[derive(Default)]
pub struct ToStringSettingsAndData(pub ToStringSettings, pub ExtractedFunctions);

/// Settings for serializing ASTNodes
pub struct ToStringSettings {
	/// Does not include whitespace minification
	pub pretty: bool,
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

impl Default for ToStringSettings {
	fn default() -> Self {
		ToStringSettings {
			pretty: true,
			include_types: false,
			include_decorators: false,
			include_comments: true,
			expect_jsx: false,
			expect_cursors: false,
			indent_with: "    ".to_owned(),
		}
	}
}

impl ToStringSettings {
	pub fn minified() -> Self {
		ToStringSettings {
			pretty: false,
			include_comments: false,
			indent_with: "".to_owned(),
			..Default::default()
		}
	}

	/// With typescript type syntax
	pub fn typescript() -> Self {
		ToStringSettings { include_types: true, ..Default::default() }
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

#[derive(Debug)]
pub struct ParseOutput<T>(pub T, pub ParsingState);

impl<T: ASTNode> ParseOutput<T> {
	// TODO shouldn't take owned self but `ToStringSettingsAndData` needs a owned `FunctionExtractor` for some reason
	pub fn to_string(self, settings: crate::ToStringSettings) -> String {
		self.0.to_string(&crate::ToStringSettingsAndData(settings, self.1.function_extractor))
	}
}

/// Defines common methods that would exist on a AST part include position in source, creation from reader and
/// serializing to string from settings.
///
/// TODO remove partial eq
pub trait ASTNode: Sized + Clone + PartialEq + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method from_reader
	#[cfg(target_arch = "wasm32")]
	fn from_string(
		string: String,
		settings: ParseSettings,
		source_id: SourceId,
		offset: Option<usize>,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<ParseOutput<Self>> {
		let lex_settings = lexer::LexSettings {
			include_comments: false,
			lex_jsx: settings.jsx,
			..Default::default()
		};
		let mut reader = BufferedTokenQueue::new();
		lexer::lex_source(&string, &mut reader, &lex_settings, Some(source_id), offset, cursors)?;
		let ret = Self::from_reader(&mut reader, &settings);
		if ret.is_ok() {
			reader.expect_next(TSXToken::EOS)?;
		}
		ret
	}

	/// From string, with default impl to call abstract method from_reader
	#[cfg(not(target_arch = "wasm32"))]
	fn from_string(
		source: String,
		settings: ParseSettings,
		source_id: SourceId,
		offset: Option<usize>,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<ParseOutput<Self>> {
		use std::thread;
		use tokenizer_lib::ParallelTokenQueue;

		let lex_settings = lexer::LexSettings {
			include_comments: false,
			lex_jsx: settings.jsx,
			..Default::default()
		};
		let (mut sender, mut reader) = ParallelTokenQueue::new();
		let parsing_thread = thread::spawn(move || {
			let mut state = ParsingState::default();
			let res = Self::from_reader(&mut reader, &mut state, &settings);
			if res.is_ok() {
				reader.expect_next(TSXToken::EOS)?;
			}
			res.map(|ast| ParseOutput(ast, state))
		});

		lexer::lex_source(&source, &mut sender, &lex_settings, Some(source_id), offset, cursors)?;
		drop(sender);

		parsing_thread.join().expect("Parsing panicked")
	}

	/// Returns position of node as span AS IT WAS PARSED. May be none if AST was doesn't match anything in source
	fn get_position(&self) -> Cow<Span>;

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self>;

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	);

	/// Returns structure as valid string
	fn to_string(&self, settings: &crate::ToStringSettingsAndData) -> String {
		let mut buf = String::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf
	}
}

#[derive(Default, Debug)]
pub struct ParsingState {
	pub function_extractor: extractor::ExtractedFunctions,
	pub hoisted_functions: HashMap<BlockId, Vec<FunctionId<StatementFunctionBase>>>,
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
		_functions: &mut ExtractedFunctions,
		chain: &mut Annex<Chain>,
	) {
		visitors.visit_keyword(&(self.0.into(), &self.1), data, _functions, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_settings: &VisitSettings,
		_functions: &mut ExtractedFunctions,
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
	type Err = ();

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
				Some('.') => Ok(Self::Number(sign.apply(s.parse().map_err(|_| ())?))),
				// TODO this is broken, needs to hex decoding etc
				Some('X' | 'x') => Ok(Self::Hex(sign, s[2..].parse().map_err(|_| ())?)),
				Some('b' | 'B') => Ok(Self::Bin(sign, s[2..].parse().map_err(|_| ())?)),
				Some('o' | 'O') => Ok(Self::Octal(sign, s[2..].parse().map_err(|_| ())?)),
				Some(_) => Ok(Self::Octal(sign, s[1..].parse().map_err(|_| ())?)),

				None => Ok(Self::Number(0.)),
			}
		} else {
			Ok(Self::Number(sign.apply(s.parse().map_err(|_| ())?)))
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
		settings: &ParseSettings,
	) -> ParseResult<(Self::Name, Option<Vec<GenericTypeConstraint>>)>;

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
		settings: &ParseSettings,
	) -> ParseResult<(Self::Name, Option<Vec<GenericTypeConstraint>>)> {
		let type_declaration = TypeDeclaration::from_reader(reader, state, settings)?;
		Ok((
			VariableIdentifier::Standard(
				type_declaration.name,
				VariableId::new(),
				type_declaration.position,
			),
			type_declaration.type_parameters,
		))
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
		settings: &ParseSettings,
	) -> ParseResult<(Self::Name, Option<Vec<GenericTypeConstraint>>)> {
		if let Token(TSXToken::OpenBrace, _) = reader.peek().unwrap() {
			Ok((None, None))
		} else {
			StatementPosition::from_reader(reader, state, settings)
				.map(|(name, constraints)| (Some(name), constraints))
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
	settings: &ParseSettings,
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
	settings: &crate::ToStringSettingsAndData,
	depth: u8,
) {
	buf.push(brackets.0);
	for (at_end, node) in nodes.iter().endiate() {
		node.to_string_from_buffer(buf, settings, depth);
		if !at_end {
			buf.push(',');
			settings.0.add_gap(buf);
		}
	}
	buf.push(brackets.1);
}

/// Re-exports or generator and general use
pub mod ast {
	pub use crate::{
		expressions::*, extensions::jsx::*, statements::*, Keyword, NumberStructure, VariableField,
		VariableId, VariableIdentifier, WithComment,
	};

	pub use self::assignments::{LHSOfAssignment, VariableOrPropertyAccess};
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) mod test_utils {
	#[macro_export]
	macro_rules! assert_matches_ast {
		($source:literal, $ast_pattern:pat) => {{
			let crate::ParseOutput(node, _state) = ASTNode::from_string(
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

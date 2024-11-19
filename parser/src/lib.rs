#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default, clippy::too_many_lines)]
#![warn(clippy::must_use_candidate)]
#![allow(unused)]

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
pub mod number;
pub mod options;
pub mod property_key;
pub mod statements;
pub mod types;
mod variable_fields;
pub mod visiting;

pub mod new {
	pub use super::lexer::Lexer;
}

pub use block::{Block, BlockLike, BlockLikeMut, BlockOrSingleStatement, StatementOrDeclaration};
pub use comments::WithComment;
pub use declarations::Declaration;
use functions::FunctionBody;
pub use marker::Marker;

pub use errors::{ParseError, ParseErrors, ParseResult};
pub use expressions::{Expression, PropertyReference};
pub use extensions::{
	decorators::{Decorated, Decorator},
	is_expression, jsx,
};
pub use functions::{FunctionBase, FunctionBased, FunctionHeader};
pub use generator_helpers::IntoAST;
use iterator_endiate::EndiateIteratorExt;
pub use modules::Module;
pub use options::*;
pub use property_key::PropertyKey;
pub use source_map::{self, SourceId, Span};
pub use statements::Statement;
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, TypeParameter},
};
pub use variable_fields::*;
pub(crate) use visiting::{
	Chain, ChainVariable, VisitOptions, Visitable, VisitorMutReceiver, VisitorReceiver,
};

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

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self>;

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

#[doc(hidden)]
pub fn lex_and_parse_script<T: ASTNode>(
	line_starts: source_map::LineStarts,
	options: ParseOptions,
	script: &str,
	offset: Option<u32>,
) -> ParseResult<(T, ParsingState)> {
	#[allow(clippy::cast_possible_truncation)]
	let length_of_source = script.len() as u32;

	let mut state = ParsingState {
		line_starts,
		length_of_source,
		constant_imports: Default::default(),
		keyword_positions: options.record_keyword_positions.then_some(KeywordPositions::new()),
		partial_points: Default::default(),
	};
	let mut lexer = crate::new::Lexer::new(script, offset, options);

	T::from_reader(&mut lexer).map(|ok| (ok, state))
}

// pub(crate) fn throw_unexpected_token<T>(
// 	reader: &mut impl TokenReader<TSXToken, TokenStart>,
// 	expected: &[TSXToken],
// ) -> ParseResult<T> {
// 	throw_unexpected_token_with_token(reader.next().unwrap(), expected)
// }

// pub(crate) fn throw_unexpected_token_with_token<T>(
// 	token: Token<TSXToken, TokenStart>,
// 	expected: &[TSXToken],
// ) -> ParseResult<T> {
// 	let position = token.get_span();
// 	Err(ParseError::new(ParseErrors::UnexpectedToken { expected, found: token.0 }, position))
// }

#[derive(Debug)]
pub struct ParsingState {
	pub line_starts: source_map::LineStarts,
	pub length_of_source: u32,
	/// TODO as multithreaded channel + record is dynamic exists
	pub constant_imports: Vec<String>,
	pub keyword_positions: Option<KeywordPositions>,
	pub partial_points: Vec<source_map::Start>,
}

// impl ParsingState {
// 	pub(crate) fn expect_keyword(
// 		&mut self,
// 		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
// 		kw: TSXKeyword,
// 	) -> crate::ParseResult<TokenStart> {
// 		let start = reader.expect(TSXToken::Keyword(kw))?;
// 		self.append_keyword_at_pos(start.0, kw);
// 		Ok(start)
// 	}

// 	pub(crate) fn optionally_expect_keyword(
// 		&mut self,
// 		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
// 		kw: TSXKeyword,
// 	) -> Option<Span> {
// 		if let Some(Token(t, start)) = reader.conditional_next(|t| *t == TSXToken::Keyword(kw)) {
// 			self.append_keyword_at_pos(start.0, kw);
// 			Some(start.with_length(t.length() as usize))
// 		} else {
// 			None
// 		}
// 	}

// 	pub(crate) fn expect_keyword_get_full_span(
// 		&mut self,
// 		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
// 		kw: TSXKeyword,
// 	) -> crate::ParseResult<Span> {
// 		let start = reader.expect(TSXToken::Keyword(kw))?;
// 		self.append_keyword_at_pos(start.0, kw);
// 		Ok(start.with_length(kw.length() as usize))
// 	}

// 	fn append_keyword_at_pos(&mut self, start: u32, kw: TSXKeyword) {
// 		if let Some(ref mut keyword_positions) = self.keyword_positions {
// 			keyword_positions.0.push((start, kw));
// 		}
// 	}

// 	fn new_partial_point_marker<T>(&mut self, at: source_map::Start) -> Marker<T> {
// 		let id = self.partial_points.len();
// 		self.partial_points.push(at);
// 		Marker(u8::try_from(id).expect("more than 256 markers"), Default::default())
// 	}
// 2}

/// As parsing is forwards, this is ordered
type TSXKeyword = &'static str;

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
			if kw_pos <= pos && pos < (kw_pos + kw.len() as u32) {
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

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition:
	Clone + std::fmt::Debug + Sync + Send + PartialEq + 'static
{
	type FunctionBody: ASTNode;

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self>;
	fn class_name_from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self>;

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
	pub is_declare: bool,
}

impl ExpressionOrStatementPosition for StatementPosition {
	type FunctionBody = FunctionBody;

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		VariableIdentifier::from_reader(reader)
			.map(|identifier| Self { identifier, is_declare: false })
	}

	fn class_name_from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		Self::from_reader(reader)
	}

	fn as_option_variable_identifier(&self) -> Option<&VariableIdentifier> {
		Some(&self.identifier)
	}

	fn as_option_variable_identifier_mut(&mut self) -> Option<&mut VariableIdentifier> {
		Some(&mut self.identifier)
	}

	fn has_function_body(body: &Self::FunctionBody) -> bool {
		body.has_body()
	}

	fn is_declare(&self) -> bool {
		self.is_declare
	}
}

#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub struct ExpressionPosition(pub Option<VariableIdentifier>);

impl ExpressionOrStatementPosition for ExpressionPosition {
	type FunctionBody = Block;

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		reader.skip();
		let is_not_name = reader.is_finished() || reader.is_one_of(&["(", "{", "[", "<"]).is_some();
		let inner = if is_not_name { None } else { Some(VariableIdentifier::from_reader(reader)?) };
		Ok(Self(inner))
	}

	fn class_name_from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		reader.skip();
		// TODO "implements" is TS syntax (reader options)
		let is_not_name = reader.is_finished()
			|| reader.is_one_of_keywords(&["extends", "implements"]).is_some()
			|| reader.is_one_of(&["(", "{", "[", "<"]).is_some();
		let inner = if is_not_name { None } else { Some(VariableIdentifier::from_reader(reader)?) };
		Ok(Self(inner))
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
	const LAST_PREFIX: Option<&'static str> = None;

	#[allow(unused)]
	fn parse_last_item(reader: &mut crate::new::Lexer) -> ParseResult<Self::LAST> {
		unreachable!("ListItem::LAST != ASTNode")
	}
}

/// Parses items surrounded in `{`, `[`, `(`, etc.
///
/// Supports trailing commas. But **does not create** *empty* like items afterwards
pub(crate) fn bracketed_items_from_reader<T: ASTNode + ListItem>(
	reader: &mut crate::new::Lexer,
	end: &'static str,
) -> ParseResult<(Vec<T>, Option<T::LAST>)> {
	// 	if let Some(start) = start {
	// 		let _ = reader.expect(start)?;
	// 	}
	let mut nodes: Vec<T> = Vec::new();
	loop {
		if reader.is_operator_advance(end) {
			return Ok((nodes, None));
		}

		if T::LAST_PREFIX.is_some_and(|l| reader.starts_with_str(l)) {
			let last = T::parse_last_item(reader)?;
			reader.expect_operator(end)?;
			return Ok((nodes, Some(last)));
		}

		let node = T::from_reader(reader)?;
		nodes.push(node);

		if reader.is_operator_advance(",") {
			continue;
		}

		if reader.is_operator_advance(end) {
			return Ok((nodes, None));
		} else {
			let current = reader.get_current();
			let position = reader.get_start().with_length(1);
			return Err(ParseError::new(
				ParseErrors::UnexpectedCharacter {
					expected: &[','],
					found: current.chars().next(),
				},
				position,
			));
		}
	}
}

/// *`to_strings`* items surrounded in `{`, `[`, `(`, etc. Defaults to `,` as delimiter
pub(crate) fn bracketed_items_to_string<T: source_map::ToString, U: ASTNode>(
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum VariableKeyword {
	Const,
	Let,
	Var,
}

impl VariableKeyword {
	// #[must_use]
	// pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
	// 	matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let | TSXKeyword::Var))
	// }

	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		if reader.is_keyword_advance("const") {
			Ok(Self::Const)
		} else if reader.is_keyword_advance("let") {
			Ok(Self::Let)
		} else if reader.is_keyword_advance("var") {
			Ok(Self::Var)
		} else {
			let current = reader.get_current();
			todo!("error here {:?}", current.get(..20).unwrap_or(current));
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
		number::NumberRepresentation,
		statements::*,
		Block, Decorated, ExpressionPosition, PropertyKey, StatementOrDeclaration,
		StatementPosition, VariableField, VariableIdentifier, WithComment,
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

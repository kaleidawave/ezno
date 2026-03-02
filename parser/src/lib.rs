#![doc = include_str!("../README.md")]
#![allow(clippy::new_without_default, clippy::too_many_lines)]
#![warn(clippy::must_use_candidate)]

mod block;
mod errors;
pub mod expressions;
pub mod extensions;
pub mod functions;
pub mod generator_helpers;
mod lexer;
pub mod marker;
pub mod modules;
pub mod numbers;
pub mod options;
pub mod property_key;
pub mod statements_and_declarations;
pub mod strings;
pub mod types;
pub mod variable_fields;
pub mod visiting;

pub use block::{Block, BlockLike, BlockLikeMut, BlockOrSingleStatement};
pub use marker::Marker;

pub use errors::{ParseError, ParseErrors, ParseResult};
pub use expressions::{Expression, MultipleExpression, PropertyReference, operators};
pub use functions::FunctionBody;
pub use functions::{FunctionBase, FunctionBased, FunctionHeader};
pub use generator_helpers::IntoAST;
pub use modules::Module;
pub use options::*;
pub use property_key::PropertyKey;
pub use source_map::{self, SourceId, Span};
pub use statements_and_declarations::{Statement, StatementOrDeclaration, control_flow, variables};
pub use strings::Quoting;
pub use types::{
	type_annotations::{self, TypeAnnotation},
	type_declarations::{self, TypeParameter},
};
pub use variable_fields::{VariableField, VariableIdentifier};

pub(crate) use lexer::Lexer;
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

// TODO state for "use strict" etc?
// TODO hold Keywords map, markers, syntax errors etc
#[derive(Default, Debug)]
pub struct ParseState {
	blank_lines: u32,
	comment_lines: u32,
	pub markers: Vec<Span>,
	pub constant_imports: Vec<String>,
	/// the current position into the script
	pub head: u32,
	pub last: u32,
}

/// Defines common methods that would exist on a AST part include position in source, creation from reader and
/// serializing to string from options.
pub trait ASTNode: Sized + Clone + std::fmt::Debug + Sync + Send + 'static {
	/// From string, with default impl to call abstract method `from_reader`
	fn from_string(script: String) -> ParseResult<Self> {
		Self::from_string_with_options(script, ParseOptions::default()).map(|(ast, _)| ast)
	}

	fn from_string_with_options(
		script: String,
		options: ParseOptions,
	) -> ParseResult<(Self, ParseState)> {
		let mut reader = crate::Lexer::new(&script, options.features.position_offset, options);
		reader.skip_including_comments();

		let node = Self::from_reader(&mut reader)?;

		if options.features.section_of_source || reader.is_finished() {
			Ok((node, reader.state))
		} else {
			let (found, position) = crate::lexer::utilities::next_item(&reader);
			Err(crate::ParseError::new(crate::ParseErrors::ExpectedEndOfSource { found }, position))
		}
	}

	/// Returns position of node as span AS IT WAS PARSED. May be `Span::NULL` if AST was doesn't match anything in source
	fn get_position(&self) -> Span;

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self>;

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

/// Classes and `function` functions have two variants depending whether in statement position
/// or expression position
pub trait ExpressionOrStatementPosition: Clone + std::fmt::Debug + Sync + Send + 'static {
	type FunctionBody: ASTNode;

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self>;

	fn class_name_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self>;

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

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct StatementPosition {
	pub identifier: VariableIdentifier,
	pub is_declare: bool,
}

impl ExpressionOrStatementPosition for StatementPosition {
	type FunctionBody = FunctionBody;

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		VariableIdentifier::from_reader(reader)
			.map(|identifier| Self { identifier, is_declare: false })
	}

	fn class_name_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
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

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct ExpressionPosition(pub Option<VariableIdentifier>);

impl ExpressionOrStatementPosition for ExpressionPosition {
	type FunctionBody = Block;

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let is_not_name = reader.is_finished() || reader.is_one_of(&["(", "{", "[", "<"]).is_some();
		let inner = if is_not_name { None } else { Some(VariableIdentifier::from_reader(reader)?) };
		Ok(Self(inner))
	}

	fn class_name_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// TODO "implements" is TS syntax (reader options)
		let is_not_name = reader.is_finished()
			|| reader.is_keyword("extends")
			|| reader.is_keyword("implements")
			|| reader.get_current().starts_with(['(', '{', '[', '<']);
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
	fn parse_last_item(reader: &mut crate::Lexer) -> ParseResult<Self::LAST> {
		unreachable!("ListItem::LAST != ASTNode")
	}

	#[must_use]
	fn skip_trailing() -> bool {
		true
	}
}

/// Parses items surrounded in `{`, `[`, `(`, etc.
///
/// Supports trailing commas. But **does not create** *empty* like items afterwards
///
/// Expects that the start character has been read
pub(crate) fn bracketed_items_from_reader<T: ASTNode + ListItem>(
	reader: &mut crate::Lexer,
	end: &'static str,
) -> ParseResult<(Vec<T>, Option<T::LAST>)> {
	let mut nodes: Vec<T> = Vec::new();
	loop {
		if (T::skip_trailing() || nodes.is_empty()) && reader.is_operator_advance(end) {
			return Ok((nodes, None));
		}

		if T::LAST_PREFIX.is_some_and(|l| reader.starts_with_slice(l)) {
			let last = T::parse_last_item(reader)?;
			reader.expect_operator(end)?;
			return Ok((nodes, Some(last)));
		}

		let node = T::from_reader(reader)?;
		nodes.push(node);

		if reader.is_operator_advance(",") {
			continue;
		}

		return if reader.is_operator_advance(end) {
			Ok((nodes, None))
		} else {
			let (found, position) = crate::lexer::utilities::next_item(reader);
			Err(ParseError::new(
				ParseErrors::UnexpectedCharacter { expected: &[','], found: found.chars().next() },
				position,
			))
		};
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
	use iterator_endiate::EndiateIteratorExt;

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
	// TODO improve
	pub use crate::{
		Block, ExpressionPosition, PropertyKey, StatementOrDeclaration, StatementPosition,
		VariableField, VariableIdentifier,
		expressions::*,
		extensions::decorators::Decorated,
		extensions::jsx::*,
		functions::{
			FunctionBase, FunctionBody, FunctionHeader, FunctionParameters, MethodHeader,
			Parameter, ParameterData, SpreadParameter,
		},
		numbers::NumberRepresentation,
		statements_and_declarations::classes::*,
		statements_and_declarations::*,
		variable_fields::*,
	};

	pub use source_map::{BaseSpan, SourceId};

	pub use self::assignments::{LHSOfAssignment, VariableOrPropertyAccess};
}

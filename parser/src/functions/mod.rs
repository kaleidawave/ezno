use std::{fmt::Debug, marker::PhantomData};

use crate::{
	bracketed_items_from_reader, bracketed_items_to_string, derive_ASTNode,
	visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty},
	ASTNode, Block, ExpressionOrStatementPosition, ExpressionPosition, ParseResult, TypeAnnotation,
	TypeParameter, VisitOptions, Visitable,
};

use derive_partial_eq_extras::PartialEqExtras;
use source_map::{Nullable, Span, ToString};

pub mod parameters;
pub use parameters::*;

pub use crate::expressions::ArrowFunction;

pub mod bases {
	pub use crate::{
		expressions::{
			arrow_function::ArrowFunctionBase, object_literal::ObjectLiteralMethodBase,
			ExpressionFunctionBase,
		},
		statements_and_declarations::{
			classes::{ClassConstructorBase, ClassFunctionBase},
			StatementFunctionBase,
		},
	};
}

pub type HeadingAndPosition<T> = <T as FunctionBased>::Header;

/// Specialization information for [`FunctionBase`]
pub trait FunctionBased: Debug + Clone + PartialEq + Send + Sync {
	/// Includes a keyword and/or modifiers
	type Header: Debug + Clone + PartialEq + Send + Sync;

	/// A name of the function
	type Name: Debug + Clone + PartialEq + Send + Sync;

	/// For `this` constraint
	#[cfg(not(feature = "serde-serialize"))]
	type LeadingParameter: LeadingParameter;

	/// Cfg to make up for the fact `serde_derive` does not use `syn_helpers`
	#[cfg(feature = "serde-serialize")]
	type LeadingParameter: LeadingParameter + serde::Serialize;

	/// For constructors
	#[cfg(not(feature = "serde-serialize"))]
	type ParameterVisibility: ParameterVisibility;

	/// Cfg to make up for the fact `serde_derive` does not use `syn_helpers`
	#[cfg(feature = "serde-serialize")]
	type ParameterVisibility: ParameterVisibility + serde::Serialize;

	/// The body of the function
	type Body: ASTNode;

	/// For debugging only
	fn get_name(name: &Self::Name) -> Option<&str>;

	fn header_and_name_from_reader(
		reader: &mut crate::Lexer,
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
	fn get_parameter_body_boundary_slice() -> Option<&'static str> {
		None
	}

	/// For overloading
	#[must_use]
	fn has_body(_: &Self::Body) -> bool {
		true
	}

	/// From TypeScript
	#[must_use]
	fn is_declare(_: &Self::Name) -> bool {
		false
	}

	/// For [`crate::ArrowFunction`]
	fn parameters_from_reader(
		reader: &mut crate::Lexer,
	) -> ParseResult<FunctionParameters<Self::LeadingParameter, Self::ParameterVisibility>> {
		FunctionParameters::from_reader(reader)
	}

	/// For [`crate::ArrowFunction`]
	fn parameters_to_string_from_buffer<T: ToString>(
		buf: &mut T,
		parameters: &FunctionParameters<Self::LeadingParameter, Self::ParameterVisibility>,
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
		options.push_gap_optionally(buf);
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
	pub type_parameters: Option<Vec<TypeParameter>>,
	pub parameters: FunctionParameters<T::LeadingParameter, T::ParameterVisibility>,
	pub return_type: Option<TypeAnnotation>,
	pub body: T::Body,
	pub position: Span,
}

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const TYPES: &str = r"
	export interface FunctionBase {
		type_parameters?: TypeParameter[],
		return_type?: TypeAnnotation,
		position: Span
	}
";

impl<T: FunctionBased> Eq for FunctionBase<T> {}

impl<T: FunctionBased + 'static> ASTNode for FunctionBase<T> {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let (header, name) = T::header_and_name_from_reader(reader)?;
		Self::from_reader_with_header_and_name(reader, header, name)
	}

	fn to_string_from_buffer<TS: source_map::ToString>(
		&self,
		buf: &mut TS,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if !options.include_type_annotations {
			if T::is_declare(&self.name) {
				return;
			}
			// Don't print overloads
			#[cfg(feature = "full-typescript")]
			if !T::has_body(&self.body) {
				return;
			}
		}

		T::header_and_name_to_string_from_buffer(buf, &self.header, &self.name, options, local);
		if let (true, Some(type_parameters)) =
			(options.include_type_annotations, &self.type_parameters)
		{
			bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
		}
		T::parameters_to_string_from_buffer(buf, &self.parameters, options, local);
		if let (true, Some(return_type)) = (options.include_type_annotations, &self.return_type) {
			buf.push_str(": ");
			return_type.to_string_from_buffer(buf, options, local);
		}
		if T::has_body(&self.body) {
			T::parameter_body_boundary_token_to_string_from_buffer(buf, options);
		}
		self.body.to_string_from_buffer(buf, options, local.next_level());
	}
}

#[allow(clippy::similar_names)]
impl<T: FunctionBased> FunctionBase<T> {
	pub(crate) fn from_reader_with_header_and_name(
		reader: &mut crate::Lexer,
		header: T::Header,
		name: T::Name,
	) -> ParseResult<Self> {
		// TODO header.get_start else here
		let start = reader.get_start();
		let type_parameters = if reader.is_operator_advance("<") {
			Some(bracketed_items_from_reader(reader, ">").map(|(params, _)| params)?)
		} else {
			None
		};
		let parameters = T::parameters_from_reader(reader)?;
		let return_type = if reader.is_operator_advance(":") {
			let precedence = if let Some("=>") = T::get_parameter_body_boundary_slice() {
				crate::types::type_annotations::TypeOperatorKind::ReturnType
			} else {
				crate::types::type_annotations::TypeOperatorKind::None
			};
			Some(TypeAnnotation::from_reader_with_precedence(reader, precedence)?)
		} else {
			None
		};
		// TODO options.type_annotations
		if let Some(slice) = T::get_parameter_body_boundary_slice() {
			reader.expect_operator(slice)?;
		}
		let body = T::Body::from_reader(reader)?;
		let body_pos = body.get_position();
		// TODO body.is_null
		let end_pos = if body_pos.is_null() {
			return_type.as_ref().map_or(parameters.position, ASTNode::get_position)
		} else {
			body_pos
		};

		let position = start.union(end_pos);

		Ok(Self { header, name, type_parameters, parameters, return_type, body, position })
	}
}

/// Visiting logic: TODO make visiting macro better and remove
impl<T: FunctionBased> Visitable for FunctionBase<T>
where
	T::Body: Visitable,
	// T::Parameters: Visitable,
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
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct GeneralFunctionBase<T: ExpressionOrStatementPosition>(PhantomData<T>);

pub type ExpressionFunction = FunctionBase<GeneralFunctionBase<ExpressionPosition>>;
#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const TYPES_EXPRESSION_FUNCTION: &str = r"
	export interface ExpressionFunction extends FunctionBase {
		header: FunctionHeader,
		body: Block,
		name: ExpressionPosition,
		parameters: FunctionParameters<null, null>
	}
";

#[allow(clippy::similar_names)]
impl<T: ExpressionOrStatementPosition> FunctionBased for GeneralFunctionBase<T> {
	type Name = T;
	type Header = FunctionHeader;
	type LeadingParameter = Option<ThisParameter>;
	type ParameterVisibility = ();
	type Body = T::FunctionBody;

	fn has_body(body: &Self::Body) -> bool {
		T::has_function_body(body)
	}

	fn is_declare(name: &Self::Name) -> bool {
		name.is_declare()
	}

	fn header_and_name_from_reader(
		reader: &mut crate::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let header = FunctionHeader::from_reader(reader)?;
		let name = T::from_reader(reader)?;
		Ok((header, name))
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
#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub enum FunctionLocationModifier {
	Server,
	Worker,
	Test,
}

#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub enum FunctionHeader {
	VirginFunctionHeader {
		is_async: bool,
		#[cfg(feature = "extras")]
		location: Option<FunctionLocationModifier>,
		is_generator: bool,
		position: Span,
	},
	/// Always is_generator
	#[cfg(feature = "extras")]
	ChadFunctionHeader {
		is_async: bool,
		// is_generator: bool,
		location: Option<FunctionLocationModifier>,
		position: Span,
	},
}

impl ASTNode for FunctionHeader {
	fn get_position(&self) -> Span {
		match self {
			FunctionHeader::VirginFunctionHeader { position, .. } => *position,
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { position, .. } => *position,
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		#[cfg(feature = "extras")]
		fn parse_location(reader: &mut crate::Lexer) -> Option<FunctionLocationModifier> {
			if reader.is_keyword_advance("server") {
				Some(FunctionLocationModifier::Server)
			} else if reader.is_keyword_advance("worker") {
				Some(FunctionLocationModifier::Worker)
			} else if reader.is_keyword_advance("test") {
				Some(FunctionLocationModifier::Test)
			} else {
				None
			}
		}

		let start = reader.get_start();
		let is_async = reader.is_keyword_advance("async");

		#[cfg(feature = "extras")]
		if reader.get_options().custom_function_headers && reader.is_keyword_advance("generator") {
			let location = parse_location(reader);
			let _ = reader.expect_keyword("function")?;
			return Ok(Self::ChadFunctionHeader {
				is_async,
				location,
				position: start.union(reader.get_end()),
			});
		}

		#[cfg(feature = "extras")]
		let location = if reader.get_options().custom_function_headers {
			parse_location(reader)
		} else {
			None
		};
		let _ = reader.expect_keyword("function")?;
		let is_generator = reader.is_operator_advance("*");
		Ok(Self::VirginFunctionHeader {
			is_async,
			is_generator,
			position: start.union(reader.get_end()),
			#[cfg(feature = "extras")]
			location,
		})
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

impl FunctionHeader {
	#[must_use]
	pub fn is_generator(&self) -> bool {
		match self {
			FunctionHeader::VirginFunctionHeader { is_generator, .. } => *is_generator,
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { .. } => true,
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

	#[must_use]
	pub fn empty() -> Self {
		Self::VirginFunctionHeader {
			is_async: false,
			#[cfg(feature = "extras")]
			location: None,
			is_generator: false,
			position: source_map::Nullable::NULL,
		}
	}
}

/// This structure removes possible invalid combinations with async
#[derive(Eq, PartialEq, Clone, Debug)]
#[apply(derive_ASTNode)]
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

// Not ASTNode as no position
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

	pub(crate) fn from_reader(reader: &mut crate::Lexer) -> Self {
		if let Some('<' | '(' | '}' | ',' | ':') =
			reader.after_identifier().trim_start().chars().next()
		{
			MethodHeader::default()
		} else if let Some(kind) = reader.is_one_of_keywords_advance(&["get", "set"]) {
			match kind {
				"get" => MethodHeader::Get,
				"set" => MethodHeader::Set,
				slice => unreachable!("{slice:?}"),
			}
		} else {
			let is_async = reader.is_keyword_advance("async");
			let generator = GeneratorSpecifier::from_reader(reader);
			MethodHeader::Regular { is_async, generator }
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
#[apply(derive_ASTNode)]
pub enum GeneratorSpecifier {
	Star(Span),
	#[cfg(feature = "extras")]
	Keyword,
}

impl GeneratorSpecifier {
	pub(crate) fn from_reader(reader: &mut crate::Lexer) -> Option<Self> {
		let start = reader.get_start();
		if reader.is_operator_advance("*") {
			Some(GeneratorSpecifier::Star(start.with_length(1)))
		} else {
			None
		}
		// TODO
		// 	#[cfg(feature = "extras")]
		// 	Some(Token(TSXToken::Keyword(TSXKeyword::Generator), _)) => Some(GeneratorSpecifier::Keyword),
	}
}

/// None if overloaded (declaration only)
#[cfg(feature = "full-typescript")]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, visitable_derive::Visitable)]
pub struct FunctionBody(pub Option<Block>);

#[cfg(not(feature = "full-typescript"))]
pub type FunctionBody = Block;

#[cfg(feature = "full-typescript")]
impl ASTNode for FunctionBody {
	fn get_position(&self) -> Span {
		self.0.as_ref().map_or(source_map::Nullable::NULL, |Block(_, pos)| *pos)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// If type annotations. Allow elided bodies for function overloading
		let body = if reader.is_operator("{") || !reader.get_options().type_annotations {
			Some(Block::from_reader(reader)?)
		} else {
			None
		};
		Ok(Self(body))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(ref b) = self.0 {
			b.to_string_from_buffer(buf, options, local);
		}
	}
}

impl FunctionBody {
	#[cfg(feature = "full-typescript")]
	#[must_use]
	pub fn has_body(&self) -> bool {
		self.0.is_some()
	}

	#[cfg(not(feature = "full-typescript"))]
	pub fn has_body(&self) -> bool {
		true
	}
}

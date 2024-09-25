use std::{fmt::Debug, marker::PhantomData};

use crate::{
	bracketed_items_from_reader, bracketed_items_to_string, derive_ASTNode,
	property_key::{PropertyKey, PropertyKeyKind},
	visiting::{ImmutableVariableOrProperty, MutableVariableOrProperty},
	ASTNode, Block, ExpressionOrStatementPosition, ExpressionPosition, ParseOptions, ParseResult,
	TypeAnnotation, TypeParameter, VisitOptions, Visitable, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use source_map::{Nullable, Span, ToString};

mod parameters;
pub use parameters::*;

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

pub type HeadingAndPosition<T> = (Option<source_map::Start>, <T as FunctionBased>::Header);

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
		reader: &mut crate::new::Lexer,
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

	/// For [`crate::ArrowFunction`]
	fn parameters_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<FunctionParameters<Self::LeadingParameter, Self::ParameterVisibility>> {
		todo!()
		// FunctionParameters::from_reader(reader)
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

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let (header_and_left, name) = T::header_and_name_from_reader(reader)?;
		Self::from_reader_with_header_and_name(reader, header_and_left, name)
	}

	fn to_string_from_buffer<TS: source_map::ToString>(
		&self,
		buf: &mut TS,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		// Don't print overloads
		#[cfg(feature = "full-typescript")]
		if !options.include_type_annotations && !T::has_body(&self.body) {
			return;
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
		reader: &mut crate::new::Lexer,
		(header_start, header): (Option<source_map::Start>, T::Header),
		name: T::Name,
	) -> ParseResult<Self> {
		let type_parameters = if reader.is_operator_advance("<") {
			Some(bracketed_items_from_reader(reader, ">").map(|(params, _)| params)?)
		} else {
			None
		};
		let parameters = FunctionParameters::from_reader(reader)?;
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

		let position =
			header_start.unwrap_or_else(|| parameters.position.get_start()).union(end_pos);

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

	fn header_and_name_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let header = FunctionHeader::from_reader(reader)?;
		let name = T::from_reader(reader)?;
		Ok(((Some(header.get_position().get_start()), header), name))
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
}

#[derive(Debug, PartialEq, Clone)]
#[apply(derive_ASTNode)]
pub enum FunctionHeader {
	VirginFunctionHeader {
		is_async: bool,
		#[cfg(feature = "extras")]
		location: Option<FunctionLocationModifier>,
		generator_star_token_position: Option<Span>,
		position: Span,
	},
	#[cfg(feature = "extras")]
	ChadFunctionHeader {
		is_async: bool,
		is_generator: bool,
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

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let is_async = reader.is_operator_advance("async");

		reader.expect_keyword("function")?;

		let generator_star_token_position = if reader.is_operator("*") {
			let position = reader.get_start().with_length(1);
			reader.advance(1);
			Some(position)
		} else {
			None
		};

		// TODO
		Ok(Self::VirginFunctionHeader {
			is_async,
			// TODO first thing
			// Option<FunctionLocationModifier>,
			location: None,
			generator_star_token_position,
			position: start.union(reader.get_end()),
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
			FunctionHeader::VirginFunctionHeader {
				generator_star_token_position: generator_star_token_pos,
				..
			} => generator_star_token_pos.is_some(),
			#[cfg(feature = "extras")]
			FunctionHeader::ChadFunctionHeader { is_generator, .. } => *is_generator,
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

	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> Self {
		if let Some(kind) = reader.is_one_of_keywords_advance(&["get", "set"]) {
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
	pub(crate) fn from_reader(reader: &mut crate::new::Lexer) -> Option<Self> {
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

/// Accounts for methods named `get` and `set` etc
pub(crate) fn get_method_name<T: PropertyKeyKind + 'static>(
	reader: &mut crate::new::Lexer,
) -> Result<(MethodHeader, WithComment<PropertyKey<T>>), crate::ParseError> {
	let after = reader.after_identifier();
	let function_header = if after.starts_with("<")
		|| after.starts_with("(")
		|| after.starts_with("}")
		|| after.starts_with(",")
		|| after.starts_with(":")
	{
		MethodHeader::default()
	} else {
		MethodHeader::from_reader(reader)
	};

	let key = WithComment::from_reader(reader)?;
	Ok((function_header, key))
}

// #[cfg(feature = "full-typescript")]
/// None if overloaded (declaration only)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, visitable_derive::Visitable)]
pub struct FunctionBody(pub Option<Block>);

// #[cfg(not(feature = "full-typescript"))]
// pub type FunctionBody = Block;

impl ASTNode for FunctionBody {
	fn get_position(&self) -> Span {
		self.0.as_ref().map_or(source_map::Nullable::NULL, |Block(_, pos)| *pos)
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		reader.skip();
		let body = if reader.starts_with('{') { Some(Block::from_reader(reader)?) } else { None };
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

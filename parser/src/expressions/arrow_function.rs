use visitable_derive::Visitable;

use crate::functions::{
	FunctionBased, FunctionBodyTrait, FunctionHeaderTrait, FunctionKind, FunctionParameters,
	HeadingAndPosition, Parameter, parse_function_body,
};
use crate::{
	ASTNode, Block, Expression, FunctionBase, ParseResult, Span, VariableField, VariableIdentifier,
	derive_ASTNode,
};

#[derive(Debug, Clone, Hash)]
pub struct ArrowFunctionBase;

pub type ArrowFunction = FunctionBase<ArrowFunctionBase>;

// pub struct IsAsync(bool, Span);

#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type IsAsync = bool;

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const TYPES: &str = r"
	export interface ArrowFunction extends FunctionBase {
		header: IsAsync,
		body: ExpressionOrBlock
	}
";

impl FunctionHeaderTrait for IsAsync {
	fn is_async(&self) -> bool {
		*self
	}

	fn is_generator(&self) -> bool {
		false
	}

	// fn get_position(&self) -> Span {
	// 	self.1
	// }
}

impl FunctionBodyTrait for ExpressionOrBlock {
	fn from_reader_as_function_body(
		reader: &mut crate::Lexer,
		directive_allowed: bool,
	) -> crate::ParseResult<Self> {
		if reader.is_operator("{") {
			Block::from_reader_as_function_body(reader, directive_allowed).map(Self::Block)
		} else {
			Expression::from_reader(reader).map(Box::new).map(Self::Expression)
		}
	}
}

impl FunctionBased for ArrowFunctionBase {
	type Name = ();
	type Header = IsAsync;
	type Body = ExpressionOrBlock;
	type LeadingParameter = ();
	type ParameterVisibility = ();

	fn kind() -> FunctionKind {
		FunctionKind::default()
	}

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderArrowFunction(this.body.get_block_id())
	// }

	fn header_and_name_from_reader(
		reader: &mut crate::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		Ok((reader.is_operator_advance("async"), ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		is_async: &Self::Header,
		_name: &Self::Name,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		if *is_async {
			buf.push_str("async ");
		}
	}

	fn parameters_from_reader(
		reader: &mut crate::Lexer,
	) -> ParseResult<FunctionParameters<(), ()>> {
		if reader.is_operator("(") {
			FunctionParameters::from_reader(reader)
		} else {
			let start = reader.get_start();
			let name = reader.parse_identifier("arrow function parameter", true)?.into_owned();
			let position = start.with_length(name.len());
			let name = VariableField::Name(VariableIdentifier::Standard(name, position));
			let parameters = vec![Parameter {
				visibility: (),
				name,
				type_annotation: None,
				additionally: None,
				position,
			}];
			Ok(FunctionParameters { leading: (), parameters, rest_parameter: None, position })
		}
	}

	fn parameters_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		parameters: &FunctionParameters<(), ()>,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		// Use shorthand if one parameter with no declared type
		if let ([Parameter { name, type_annotation, additionally, .. }], None) =
			(parameters.parameters.as_slice(), &parameters.rest_parameter)
		{
			let is_printing_type_annotation =
				options.include_type_annotations && type_annotation.is_some();
			if !is_printing_type_annotation
				&& !matches!(
					additionally,
					Some(crate::functions::ParameterData::WithDefaultValue(_))
				) && let VariableField::Name(name, ..) = name
			{
				name.to_string_from_buffer(buf, options, local);
				return;
			}
		}
		parameters.to_string_from_buffer(buf, options, local);
	}

	fn parameter_body_boundary_token_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		options: &crate::ToStringOptions,
	) {
		buf.push_str(if options.pretty { " => " } else { "=>" });
	}

	fn visit_name<TData>(
		(): &Self::Name,
		_: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn visit_name_mut<TData>(
		(): &mut Self::Name,
		_: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn get_name((): &Self::Name) -> Option<&str> {
		None
	}

	fn get_parameter_body_boundary_slice() -> Option<&'static str> {
		Some("=>")
	}
}

impl ArrowFunction {
	/// Given an `is_async` and an `identifier`, parses the rest of the arrow function
	///
	/// ```typescript
	/// *is_async* *identifier* =>
	/// //                      ^ expects reader just before `=>` symbol
	/// ```
	pub(crate) fn from_reader_with_first_parameter(
		reader: &mut crate::Lexer,
		is_async: bool,
		name: VariableField,
	) -> ParseResult<Self> {
		let position = name.get_position();
		let parameters = FunctionParameters {
			leading: (),
			parameters: vec![Parameter {
				name,
				position,
				visibility: (),
				type_annotation: None,
				additionally: None,
			}],
			rest_parameter: None,
			position,
		};

		Self::from_reader_with_parameters(reader, position.get_start(), is_async, None, parameters)
	}

	pub(crate) fn from_reader_with_parameters(
		reader: &mut crate::Lexer,
		start: source_map::Start,
		is_async: bool,
		type_parameters: Option<crate::functions::FunctionTypeParameters>,
		parameters: FunctionParameters<(), ()>,
	) -> ParseResult<Self> {
		let return_type = if reader.is_operator_advance(":") {
			Some(crate::types::TypeAnnotation::from_reader(reader)?)
		} else {
			None
		};
		reader.expect_operator("=>")?;
		let body = parse_function_body::<ArrowFunctionBase>(
			reader,
			&is_async,
			ArrowFunctionBase::kind(),
			!parameters.has_default_or_spread(),
		)?;
		let arrow_function = ArrowFunction {
			header: is_async,
			position: start.union(body.get_position()),
			name: (),
			parameters,
			return_type,
			type_parameters,
			body,
		};
		Ok(arrow_function)
	}
}

/// For [`ArrowFunction`] and [`crate::MatchArm`] bodies
#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum ExpressionOrBlock {
	Expression(Box<Expression>),
	Block(Block),
}

impl ASTNode for ExpressionOrBlock {
	fn get_position(&self) -> Span {
		match self {
			ExpressionOrBlock::Expression(expression) => expression.get_position(),
			ExpressionOrBlock::Block(block) => block.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.is_operator("{") {
			Block::from_reader(reader).map(Self::Block)
		} else {
			Expression::from_reader(reader).map(Box::new).map(Self::Expression)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			ExpressionOrBlock::Expression(expr) => expr.to_string_from_buffer(buf, options, local),
			ExpressionOrBlock::Block(block) => block.to_string_from_buffer(buf, options, local),
		}
	}
}

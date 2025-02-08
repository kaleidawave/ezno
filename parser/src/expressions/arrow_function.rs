use visitable_derive::Visitable;

use crate::{
	derive_ASTNode,
	functions::HeadingAndPosition,
	functions::{FunctionBased, FunctionParameters, Parameter},
	ASTNode, Block, Expression, FunctionBase, ParseResult, Span, VariableField, VariableIdentifier,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrowFunctionBase;

pub type ArrowFunction = FunctionBase<ArrowFunctionBase>;
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

impl FunctionBased for ArrowFunctionBase {
	type Name = ();
	type Header = IsAsync;
	type Body = ExpressionOrBlock;
	type LeadingParameter = ();
	type ParameterVisibility = ();

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
			let name = reader.parse_identifier("arrow function parameter", true)?;
			let position = start.with_length(name.len());
			let parameters = vec![Parameter {
				visibility: (),
				name: VariableField::Name(VariableIdentifier::Standard(name.to_owned(), position))
					.into(),
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
				) {
				if let VariableField::Name(name, ..) = name.get_ast_ref() {
					name.to_string_from_buffer(buf, options, local);
					return;
				}
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
	pub(crate) fn from_reader_with_first_parameter(
		reader: &mut crate::Lexer,
		is_async: bool,
		identifier: VariableIdentifier,
	) -> ParseResult<Self> {
		let position = identifier.get_position();
		let parameters = vec![Parameter {
			name: VariableField::Name(identifier).into(),
			position,
			visibility: (),
			type_annotation: None,
			additionally: None,
		}];
		reader.expect_operator("=>")?;
		let body = ExpressionOrBlock::from_reader(reader)?;
		let arrow_function = FunctionBase {
			header: is_async,
			position: position.union(body.get_position()),
			name: (),
			parameters: FunctionParameters {
				parameters,
				rest_parameter: None,
				position,
				leading: (),
			},
			return_type: None,
			type_parameters: None,
			body,
		};
		Ok(arrow_function)
	}
}

/// For [`ArrowFunction`] and [`crate::MatchArm`] bodies
#[derive(Debug, Clone, PartialEq, Visitable)]
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

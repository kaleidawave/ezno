use visitable_derive::Visitable;

use crate::{
	derive_ASTNode,
	functions::HeadingAndPosition,
	functions::{FunctionBased, FunctionParameters, Parameter},
	ASTNode, Block, Expression, FunctionBase, ParseOptions, ParseResult, Span, TypeAnnotation,
	VariableField, VariableIdentifier,
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
		reader: &mut crate::new::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		todo!()
		// let async_pos = state.optionally_expect_keyword(reader, crate::TSXKeyword::Async);
		// Ok(((async_pos.map(|s| s.get_start()), async_pos.is_some()), ()))
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
		reader: &mut crate::new::Lexer,
	) -> ParseResult<FunctionParameters<(), ()>> {
		todo!();
		// match reader.next().ok_or_else(parse_lexing_error)? {
		// 	Token(TSXToken::OpenParentheses, open_paren) => {
		// 		FunctionParameters::from_reader_sub_open_parenthesis(
		// 			reader, state, options, open_paren,
		// 		)
		// 	}
		// 	// `x` => ...
		// 	token => {
		// 		let (name, position) = token_as_identifier(token, "arrow function parameter")?;
		// 		let parameters = vec![Parameter {
		// 			visibility: (),
		// 			name: VariableField::Name(VariableIdentifier::Standard(name, position)).into(),
		// 			type_annotation: None,
		// 			additionally: None,
		// 			position,
		// 		}];
		// 		Ok(FunctionParameters { leading: (), parameters, rest_parameter: None, position })
		// 	}
		// }
	}

	fn parameters_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		parameters: &FunctionParameters<(), ()>,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		// Use shorthand if one parameter with no declared type
		if let (true, [Parameter { name, .. }]) =
			(parameters.rest_parameter.is_none(), parameters.parameters.as_slice())
		{
			if let VariableField::Name(name, ..) = name.get_ast_ref() {
				name.to_string_from_buffer(buf, options, local);
			} else {
				parameters.to_string_from_buffer(buf, options, local);
			}
		} else {
			parameters.to_string_from_buffer(buf, options, local);
		}
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
}

impl ArrowFunction {
	pub(crate) fn from_reader_with_first_parameter(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<Self> {
		let _existing = r#"let (first_parameter_name, first_parameter_position) = first_parameter;
		let name = VariableField::Name(VariableIdentifier::Standard(
			first_parameter_name,
			first_parameter_position,
		));
		let parameters = vec![Parameter {
			visibility: (),
			name: name.into(),
			type_annotation: None,
			additionally: None,
			position: first_parameter.1,
		}];
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, options)?;
		let arrow_function = FunctionBase {
			header: is_async,
			position: first_parameter.1.union(body.get_position()),
			name: (),
			parameters: FunctionParameters {
				parameters,
				rest_parameter: None,
				position: first_parameter.1,
				leading: (),
			},
			return_type: None,
			type_parameters: None,
			body,
		};
		Ok(arrow_function)"#;
		todo!();
	}

	pub(crate) fn from_reader_sub_open_paren(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"let parameters =
			FunctionParameters::from_reader_sub_open_parenthesis(reader, state, options, start)?;

		let return_type = if reader
			.conditional_next(|token| options.type_annotations && matches!(token, TSXToken::Colon))
			.is_some()
		{
			Some(TypeAnnotation::from_reader(reader, state, options)?)
		} else {
			None
		};
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, options)?;
		Ok(FunctionBase {
			header: is_async,
			name: (),
			parameters,
			return_type,
			type_parameters: None,
			position: start.union(body.get_position()),
			body,
		})"#;
		todo!();
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

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"if let Some(Token(TSXToken::OpenBrace, _)) = reader.peek() {
			Ok(Self::Block(Block::from_reader(reader, state, options)?))
		} else {
			let expression = Expression::from_reader(reader, state, options)?;
			Ok(Self::Expression(Box::new(expression)))
		}"#;
		todo!();
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

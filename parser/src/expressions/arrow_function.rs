use crate::{tsx_keywords, VariableIdentifier};
use tokenizer_lib::sized_tokens::TokenStart;
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, functions::FunctionBased, parameters::FunctionParameters,
	tokens::token_as_identifier, ASTNode, Block, Expression, FunctionBase, Keyword, Parameter,
	ParseOptions, ParseResult, Span, TSXToken, Token, TokenReader, TypeAnnotation, VariableField,
	WithComment,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrowFunctionBase;

pub type ArrowFunction = FunctionBase<ArrowFunctionBase>;

impl FunctionBased for ArrowFunctionBase {
	type Name = ();
	type Header = Option<Keyword<tsx_keywords::Async>>;
	type Body = ExpressionOrBlock;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderArrowFunction(this.body.get_block_id())
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let async_keyword = Keyword::optionally_from_reader(reader);
		Ok((async_keyword, ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		is_async: &Self::Header,
		_name: &Self::Name,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		if is_async.is_some() {
			buf.push_str("async ")
		}
	}

	fn parameters_from_reader<T: source_map::ToString>(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<FunctionParameters> {
		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::OpenParentheses, open_paren) => {
				FunctionParameters::from_reader_sub_open_parenthesis(
					reader, state, settings, open_paren,
				)
			}
			// `x` => ...
			token => {
				let (name, position) = token_as_identifier(token, "arrow function parameter")?;
				let parameters = vec![Parameter {
					name: WithComment::None(
						VariableIdentifier::Standard(name, position.clone()).into(),
					),
					type_annotation: None,
					additionally: None,
					position: position.clone(),
				}];
				Ok(FunctionParameters { parameters, rest_parameter: None, position })
			}
		}
	}

	fn parameters_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		parameters: &FunctionParameters,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		// Use shorthand if one parameter with no declared type
		if let (true, [Parameter { name, .. }]) =
			(parameters.rest_parameter.is_none(), parameters.parameters.as_slice())
		{
			if let VariableField::Name(name, ..) = name.get_ast_ref() {
				buf.push_str(name.as_str());
			} else {
				parameters.to_string_from_buffer(buf, settings, depth);
			}
		} else {
			parameters.to_string_from_buffer(buf, settings, depth);
		}
	}

	fn parameter_body_boundary_token_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		settings: &crate::ToStringOptions,
	) {
		buf.push_str(if settings.pretty { " => " } else { "=>" });
	}

	fn header_left(header: &Self::Header) -> Option<&Span> {
		header.as_ref().map(|kw| kw.get_position())
	}
}

impl ArrowFunction {
	pub(crate) fn from_reader_with_first_parameter(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		first_parameter: (String, Span),
	) -> ParseResult<Self> {
		let parameters = vec![crate::Parameter {
			name: WithComment::None(
				VariableIdentifier::Standard(first_parameter.0, first_parameter.1.clone()).into(),
			),
			type_annotation: None,
			additionally: None,
			position: first_parameter.1.clone(),
		}];
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, settings)?;
		let arrow_function = FunctionBase {
			header: None,
			position: first_parameter.1.union(body.get_position()),
			name: (),
			parameters: crate::FunctionParameters {
				parameters,
				rest_parameter: None,
				position: first_parameter.1,
			},
			return_type: None,
			type_parameters: None,
			body,
		};
		Ok(arrow_function)
	}

	pub(crate) fn from_reader_sub_open_paren(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		is_async: Option<Keyword<tsx_keywords::Async>>,
		start: TokenStart,
	) -> ParseResult<Self> {
		let parameters =
			FunctionParameters::from_reader_sub_open_parenthesis(reader, state, settings, start)?;

		let return_type =
			if reader.conditional_next(|token| matches!(token, TSXToken::Colon)).is_some() {
				Some(TypeAnnotation::from_reader(reader, state, settings)?)
			} else {
				None
			};
		reader.expect_next(TSXToken::Arrow)?;
		let body = ExpressionOrBlock::from_reader(reader, state, settings)?;
		Ok(FunctionBase {
			header: is_async,
			name: (),
			parameters,
			return_type,
			type_parameters: None,
			position: start.union(body.get_position()),
			body,
		})
	}
}

/// For [ArrowFunction] and [crate::MatchArm] bodies
#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ExpressionOrBlock {
	Expression(Box<Expression>),
	Block(Block),
}

impl ASTNode for ExpressionOrBlock {
	fn get_position(&self) -> &Span {
		match self {
			ExpressionOrBlock::Expression(expression) => expression.get_position(),
			ExpressionOrBlock::Block(block) => block.get_position(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::OpenBrace, _)) = reader.peek() {
			Ok(Self::Block(Block::from_reader(reader, state, settings)?))
		} else {
			let expression = Expression::from_reader(reader, state, settings)?;
			Ok(Self::Expression(Box::new(expression)))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			ExpressionOrBlock::Expression(expr) => expr.to_string_from_buffer(buf, settings, depth),
			ExpressionOrBlock::Block(block) => block.to_string_from_buffer(buf, settings, depth),
		}
	}
}

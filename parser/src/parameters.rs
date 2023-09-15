use crate::TSXToken;
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{
	sized_tokens::{TokenReaderWithTokenEnds, TokenStart},
	Token, TokenReader,
};
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression, ParseError,
	ParseResult, TypeAnnotation, VariableField, VariableFieldInSourceCode, VariableIdentifier,
	WithComment,
};

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct Parameter {
	pub name: WithComment<VariableField<VariableFieldInSourceCode>>,
	pub type_annotation: Option<TypeAnnotation>,
	pub additionally: Option<ParameterData>,
}

// TODO not sure whether parameter should implement ASTNode
impl Parameter {
	pub fn get_position(&self) -> &Span {
		todo!()
		// let position = self.name.get_position();
		// if let Some(tr) = &self.type_annotation {
		// 	position.union(tr.get_position())
		// } else {
		// 	position
		// }
	}
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ParameterData {
	Optional,
	WithDefaultValue(Box<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct SpreadParameter {
	pub name: VariableIdentifier,
	pub type_annotation: Option<TypeAnnotation>,
}

/// TODO need to something special to not enable `OptionalFunctionParameter::WithValue` in interfaces and other
/// type structure
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct FunctionParameters {
	pub parameters: Vec<Parameter>,
	pub rest_parameter: Option<Box<SpreadParameter>>,
	#[partial_eq_ignore]
	pub position: Span,
}

impl Eq for FunctionParameters {}

impl ASTNode for FunctionParameters {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
	) -> ParseResult<Self> {
		let open_paren_span = reader.expect_next(TSXToken::OpenParentheses)?;
		Self::from_reader_sub_open_parenthesis(reader, state, settings, open_paren_span)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		let FunctionParameters { parameters, rest_parameter, .. } = self;
		buf.push('(');
		for (at_end, Parameter { name, type_annotation, additionally, .. }) in
			parameters.iter().endiate()
		{
			// decorators_to_string_from_buffer(decorators, buf, settings, depth);
			name.to_string_from_buffer(buf, settings, depth);
			if let (true, Some(ref type_annotation)) = (settings.include_types, type_annotation) {
				if let Some(ParameterData::Optional) = additionally {
					buf.push('?');
				}
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, settings, depth);
			}
			if let Some(ParameterData::WithDefaultValue(value)) = additionally {
				buf.push_str(if settings.pretty { " = " } else { "=" });
				value.to_string_from_buffer(buf, settings, depth);
			}
			if !at_end || rest_parameter.is_some() {
				buf.push(',');
				settings.add_gap(buf);
			}
		}
		if let Some(rest_parameter) = rest_parameter {
			buf.push_str("...");
			buf.push_str(rest_parameter.name.as_str());
			if let Some(ref type_annotation) = rest_parameter.type_annotation {
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, settings, depth);
			}
		}
		buf.push(')');
	}
}

impl FunctionParameters {
	pub(crate) fn from_reader_sub_open_parenthesis(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseOptions,
		start: TokenStart,
	) -> Result<FunctionParameters, ParseError> {
		let mut parameters = Vec::new();
		let mut rest_parameter = None;

		loop {
			if let Some(Token(TSXToken::CloseParentheses, _)) = reader.peek() {
				break;
			}
			// Skip comments
			while reader.conditional_next(TSXToken::is_comment).is_some() {}

			if let Some(Token(_, _spread_pos)) =
				reader.conditional_next(|tok| matches!(tok, TSXToken::Spread))
			{
				let (name, name_pos) = token_as_identifier(
					reader.next().ok_or_else(parse_lexing_error)?,
					"spread function parameter",
				)?;
				let type_annotation =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						Some(TypeAnnotation::from_reader(reader, state, settings)?)
					} else {
						None
					};
				rest_parameter = Some(Box::new(SpreadParameter {
					name: VariableIdentifier::Standard(name, name_pos),
					type_annotation,
				}));
				break;
			} else {
				let name = WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
					reader, state, settings,
				)?;

				let (is_optional, type_annotation) = match reader.peek() {
					Some(Token(TSXToken::Colon, _)) => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						(false, Some(type_annotation))
					}
					Some(Token(TSXToken::OptionalMember, _)) => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						(true, Some(type_annotation))
					}
					Some(Token(TSXToken::QuestionMark, _)) => {
						let Token(_, _) = reader.next().unwrap();
						(true, None)
					}
					_ => (false, None),
				};

				let value = if let Some(token) =
					reader.conditional_next(|tok| matches!(tok, TSXToken::Assign))
				{
					if is_optional {
						return Err(ParseError::new(
							crate::ParseErrors::FunctionParameterOptionalAndDefaultValue,
							token.get_span(),
						));
					}
					Some(Box::new(Expression::from_reader(reader, state, settings)?))
				} else {
					None
				};

				let additionally = match (is_optional, value) {
					(true, Some(_)) => unreachable!("caught earlier by error"),
					// =
					(false, Some(value)) => Some(ParameterData::WithDefaultValue(value)),
					// ?:
					(true, None) => Some(ParameterData::Optional),
					(false, None) => None,
				};
				parameters.push(Parameter { name, type_annotation, additionally });
			}
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		let close = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
		Ok(FunctionParameters { position: start.union(close), parameters, rest_parameter })
	}
}

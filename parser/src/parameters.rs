use std::borrow::Cow;

use crate::TSXToken;
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};
use visitable_derive::Visitable;

use crate::{
	errors::parse_lexing_error, tokens::token_as_identifier, ASTNode, Expression, ParseError,
	ParseResult, TypeReference, VariableField, VariableFieldInSourceCode, VariableId,
	VariableIdentifier, WithComment,
};

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
pub struct Parameter {
	pub name: WithComment<VariableField<VariableFieldInSourceCode>>,
	pub type_reference: Option<TypeReference>,
}

// TODO not sure whether parameter should implement ASTNode
impl Parameter {
	pub fn get_position(&self) -> Cow<Span> {
		let position = self.name.get_position();
		if let Some(tr) = &self.type_reference {
			Cow::Owned(position.union(&tr.get_position()))
		} else {
			position
		}
	}
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
pub enum OptionalOrWithDefaultValueParameter {
	Optional {
		// WithComment<VariableField<VariableFieldInSourceCode>>
		name: VariableIdentifier,
		type_reference: Option<TypeReference>,
	},
	WithDefaultValue {
		name: WithComment<VariableField<VariableFieldInSourceCode>>,
		type_reference: Option<TypeReference>,
		value: Box<Expression>,
	},
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
pub struct SpreadParameter {
	pub name: VariableIdentifier,
	pub type_reference: Option<TypeReference>,
}

/// TODO need to something special to not enable `OptionalFunctionParameter::WithValue` in interfaces and other
/// type structure
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
pub struct FunctionParameters {
	pub parameters: Vec<Parameter>,
	pub optional_parameters: Vec<OptionalOrWithDefaultValueParameter>,
	pub rest_parameter: Option<Box<SpreadParameter>>,
	#[partial_eq_ignore]
	pub position: Span,
}

impl Eq for FunctionParameters {}

impl ASTNode for FunctionParameters {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
	) -> ParseResult<Self> {
		let open_paren_span = reader.expect_next(TSXToken::OpenParentheses)?;
		Self::from_reader_sub_open_parenthesis(reader, state, settings, open_paren_span)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		let FunctionParameters { parameters, optional_parameters, rest_parameter, .. } = self;
		buf.push('(');
		for (at_end, Parameter { name, type_reference, .. }) in parameters.iter().endiate() {
			// decorators_to_string_from_buffer(decorators, buf, settings, depth);
			name.to_string_from_buffer(buf, settings, depth);
			if let (true, Some(ref type_reference)) = (settings.include_types, type_reference) {
				buf.push_str(": ");
				type_reference.to_string_from_buffer(buf, settings, depth);
			}
			if !at_end || !optional_parameters.is_empty() || rest_parameter.is_some() {
				buf.push(',');
				settings.add_gap(buf);
			}
		}
		for (at_end, parameter) in optional_parameters.iter().endiate() {
			match parameter {
				OptionalOrWithDefaultValueParameter::Optional { name, type_reference, .. } => {
					buf.push_str(name.as_str());
					buf.push('?');
					if let (true, Some(type_reference)) = (settings.include_types, type_reference) {
						buf.push_str(": ");
						type_reference.to_string_from_buffer(buf, settings, depth);
					}
				}
				OptionalOrWithDefaultValueParameter::WithDefaultValue {
					name,
					type_reference,
					value,
					..
				} => {
					name.to_string_from_buffer(buf, settings, depth);
					if let (true, Some(type_reference)) = (settings.include_types, type_reference) {
						buf.push_str(": ");
						type_reference.to_string_from_buffer(buf, settings, depth);
					}
					buf.push_str(if settings.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, settings, depth);
				}
			}
			if !at_end || rest_parameter.is_some() {
				buf.push(',');
				settings.add_gap(buf);
			}
		}
		if let Some(rest_parameter) = rest_parameter {
			buf.push_str("...");
			buf.push_str(rest_parameter.name.as_str());
			if let Some(ref type_reference) = rest_parameter.type_reference {
				buf.push_str(": ");
				type_reference.to_string_from_buffer(buf, settings, depth);
			}
		}
		buf.push(')');
	}
}

impl FunctionParameters {
	pub(crate) fn from_reader_sub_open_parenthesis(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &crate::ParseSettings,
		start_pos: Span,
	) -> Result<FunctionParameters, ParseError> {
		let mut parameters = Vec::new();
		let mut optional_parameters = Vec::new();
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
				let type_reference =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						Some(TypeReference::from_reader(reader, state, settings)?)
					} else {
						None
					};
				rest_parameter = Some(Box::new(SpreadParameter {
					name: VariableIdentifier::Standard(name, VariableId::new(), name_pos),
					type_reference,
				}));
				break;
			} else {
				let name = WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
					reader, state, settings,
				)?;
				let (is_optional, type_reference) = match reader.peek() {
					Some(Token(TSXToken::Colon, _)) => {
						reader.next();
						let type_reference = TypeReference::from_reader(reader, state, settings)?;
						(false, Some(type_reference))
					}
					Some(Token(TSXToken::OptionalMember, _)) => {
						reader.next();
						let type_reference = TypeReference::from_reader(reader, state, settings)?;
						(true, Some(type_reference))
					}
					Some(Token(TSXToken::QuestionMark, _)) => {
						let Token(_, _) = reader.next().unwrap();
						(true, None)
					}
					_ => (false, None),
				};

				let value = if let Some(Token(_, pos)) =
					reader.conditional_next(|tok| matches!(tok, TSXToken::Assign))
				{
					if is_optional {
						return Err(ParseError::new(
							crate::ParseErrors::FunctionParameterOptionalAndDefaultValue,
							pos,
						));
					}
					Some(Expression::from_reader(reader, state, settings)?)
				} else {
					None
				};

				match (is_optional, value) {
					(true, Some(_)) => unreachable!("caught earlier by error"),
					// =
					(false, Some(value)) => {
						optional_parameters.push(
							OptionalOrWithDefaultValueParameter::WithDefaultValue {
								name,
								type_reference,
								value: Box::new(value),
							},
						);
					}
					// ?:
					(true, None) => {
						let name = if let VariableField::Name(VariableIdentifier::Standard(
							name,
							variable_id,
							position,
						)) = name.unwrap_ast()
						{
							VariableIdentifier::Standard(name, variable_id, position)
						} else {
							todo!("error")
						};
						optional_parameters.push(OptionalOrWithDefaultValueParameter::Optional {
							name,
							type_reference,
						});
					}
					(false, None) => {
						if !optional_parameters.is_empty() {
							return Err(ParseError::new(
                                crate::ParseErrors::NonOptionalFunctionParameterAfterOptionalFunctionParameter,
								name.get_position().into_owned()
                            ));
						}
						parameters.push(Parameter { name, type_reference });
					}
				}
			}
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		let end_span = reader.expect_next(TSXToken::CloseParentheses)?;
		Ok(FunctionParameters {
			position: start_pos.union(&end_span),
			parameters,
			optional_parameters,
			rest_parameter,
		})
	}
}

use crate::{derive_ASTNode, TSXKeyword, TSXToken};
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

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Eq, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Parameter {
	pub name: WithComment<VariableField<VariableFieldInSourceCode>>,
	pub type_annotation: Option<TypeAnnotation>,
	pub additionally: Option<ParameterData>,
	pub position: Span,
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ParameterData {
	Optional,
	WithDefaultValue(Box<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct SpreadParameter {
	pub name: VariableIdentifier,
	pub type_annotation: Option<TypeAnnotation>,
	pub position: Span,
}

/// TODO need to something special to not enable `OptionalFunctionParameter::WithValue` in interfaces and other
/// type structure
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
pub struct FunctionParameters {
	pub this_type: Option<(TypeAnnotation, Span)>,
	pub super_type: Option<(TypeAnnotation, Span)>,
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
		options: &crate::ParseOptions,
	) -> ParseResult<Self> {
		let open_paren_span = reader.expect_next(TSXToken::OpenParentheses)?;
		Self::from_reader_sub_open_parenthesis(reader, state, options, open_paren_span)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		let FunctionParameters { parameters, rest_parameter, .. } = self;
		buf.push('(');
		for (at_end, Parameter { name, type_annotation, additionally, .. }) in
			parameters.iter().endiate()
		{
			// decorators_to_string_from_buffer(decorators, buf, options, local);
			name.to_string_from_buffer(buf, options, local);
			if let (true, Some(ref type_annotation)) = (options.include_types, type_annotation) {
				if let Some(ParameterData::Optional) = additionally {
					buf.push('?');
				}
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, options, local);
			}
			if let Some(ParameterData::WithDefaultValue(value)) = additionally {
				buf.push_str(if options.pretty { " = " } else { "=" });
				value.to_string_from_buffer(buf, options, local);
			}
			if !at_end || rest_parameter.is_some() {
				buf.push(',');
				options.push_gap_optionally(buf);
			}
		}
		if let Some(rest_parameter) = rest_parameter {
			buf.push_str("...");
			rest_parameter.name.to_string_from_buffer(buf, options, local);
			if let Some(ref type_annotation) = rest_parameter.type_annotation {
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, options, local);
			}
		}
		buf.push(')');
	}
}

impl FunctionParameters {
	pub(crate) fn from_reader_sub_open_parenthesis(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
		start: TokenStart,
	) -> Result<FunctionParameters, ParseError> {
		let mut this_type = None;
		let mut super_type = None;
		let mut parameters = Vec::new();
		let mut rest_parameter = None;

		loop {
			if let Some(Token(TSXToken::CloseParentheses, _)) = reader.peek() {
				break;
			}
			// Skip comments
			while reader.conditional_next(TSXToken::is_comment).is_some() {}

			if let Some(Token(_, spread_pos)) =
				reader.conditional_next(|tok| matches!(tok, TSXToken::Spread))
			{
				let (name, name_pos) = token_as_identifier(
					reader.next().ok_or_else(parse_lexing_error)?,
					"spread function parameter",
				)?;
				let type_annotation = if options.type_annotations
					&& reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some()
				{
					Some(TypeAnnotation::from_reader(reader, state, options)?)
				} else {
					None
				};

				let position = spread_pos
					.union(type_annotation.as_ref().map_or(&name_pos, ASTNode::get_position));

				rest_parameter = Some(Box::new(SpreadParameter {
					name: VariableIdentifier::Standard(name, name_pos),
					type_annotation,
					position,
				}));
				break;
			} else if let Some(Token(_, start)) = reader.conditional_next(|tok| {
				options.type_annotations
					&& parameters.is_empty()
					&& matches!(tok, TSXToken::Keyword(TSXKeyword::This))
			}) {
				reader.expect_next(TSXToken::Colon)?;
				let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(type_annotation.get_position());
				this_type = Some((type_annotation, position));
			} else if let Some(Token(_, start)) = reader.conditional_next(|tok| {
				options.type_annotations
					&& parameters.is_empty()
					&& matches!(tok, TSXToken::Keyword(TSXKeyword::Super))
			}) {
				reader.expect_next(TSXToken::Colon)?;
				let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(type_annotation.get_position());
				super_type = Some((type_annotation, position));
			} else {
				let name = WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
					reader, state, options,
				)?;

				let (is_optional, type_annotation) = match reader.peek() {
					Some(Token(TSXToken::Colon, _)) if options.type_annotations => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
						(false, Some(type_annotation))
					}
					Some(Token(TSXToken::OptionalMember, _)) if options.type_annotations => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
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
					Some(Box::new(Expression::from_reader(reader, state, options)?))
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

				let end_position = if let Some(ParameterData::WithDefaultValue(e)) = &additionally {
					e.get_position()
				} else if let Some(type_annotation) = &type_annotation {
					type_annotation.get_position()
				} else {
					name.get_position()
				};

				parameters.push(Parameter {
					position: name.get_position().union(end_position),
					name,
					type_annotation,
					additionally,
				});
			}
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}
		let close = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
		Ok(FunctionParameters {
			position: start.union(close),
			parameters,
			rest_parameter,
			this_type,
			super_type,
		})
	}
}

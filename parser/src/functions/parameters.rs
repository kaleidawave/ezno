use std::fmt::Debug;

use crate::{
	derive_ASTNode, ASTNode, Expression, ParseError, ParseErrors, ParseResult, TSXKeyword,
	TSXToken, TypeAnnotation, VariableField, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use tokenizer_lib::{
	sized_tokens::{TokenReaderWithTokenEnds, TokenStart},
	Token, TokenReader,
};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Parameter<V> {
	#[visit_skip_field]
	pub visibility: V,
	pub name: WithComment<VariableField>,
	pub type_annotation: Option<TypeAnnotation>,
	pub additionally: Option<ParameterData>,
	pub position: Span,
}

pub trait ParameterVisibility: Send + Sync + Sized + Debug + PartialEq + Clone + 'static {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> Self;
}

impl ParameterVisibility for () {
	fn from_reader(
		_: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_: &mut crate::ParsingState,
		_: &crate::ParseOptions,
	) -> Self {
	}
}

impl ParameterVisibility for Option<crate::types::Visibility> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> Option<crate::types::Visibility> {
		if !options.type_annotations {
			None
		} else if let Some(Token(TSXToken::Keyword(t), _)) =
			reader.conditional_next(crate::types::Visibility::token_is_visibility_specifier)
		{
			Some(match t {
				TSXKeyword::Private => crate::types::Visibility::Private,
				TSXKeyword::Public => crate::types::Visibility::Public,
				TSXKeyword::Protected => crate::types::Visibility::Protected,
				_ => unreachable!(),
			})
		} else {
			None
		}
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ParameterData {
	Optional,
	WithDefaultValue(Box<Expression>),
}

#[cfg(feature = "extras")]
#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type SpreadParameterName = VariableField;

#[cfg(not(feature = "extras"))]
#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type SpreadParameterName = crate::VariableIdentifier;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable)]
pub struct SpreadParameter {
	pub name: SpreadParameterName,
	pub type_annotation: Option<TypeAnnotation>,
	pub position: Span,
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct FunctionParameters<L, V> {
	#[visit_skip_field]
	pub leading: L,
	pub parameters: Vec<Parameter<V>>,
	pub rest_parameter: Option<Box<SpreadParameter>>,
	pub position: Span,
}

pub trait LeadingParameter: Send + Sync + Sized + Debug + PartialEq + Clone + 'static {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self>;

	fn get_this_parameter(&self) -> Option<&ThisParameter>;
	fn get_super_parameter(&self) -> Option<&SuperParameter>;
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct ThisParameter {
	pub constraint: TypeAnnotation,
	pub position: Span,
}

/// TODO WIP!
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct SuperParameter {
	pub constraint: TypeAnnotation,
	pub position: Span,
}

impl LeadingParameter for () {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		if this_annotation.is_some() || super_annotation.is_some() {
			let position =
				this_annotation.map_or(super_annotation.unwrap().position, |a| a.position);

			Err(ParseError::new(ParseErrors::CannotUseLeadingParameterHere, position))
		} else {
			Ok(())
		}
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		None
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		None
	}
}

impl LeadingParameter for Option<ThisParameter> {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		if let Some(s) = super_annotation {
			Err(ParseError::new(ParseErrors::CannotUseLeadingParameterHere, s.position))
		} else {
			Ok(this_annotation)
		}
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		self.as_ref()
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		None
	}
}

impl LeadingParameter for (Option<ThisParameter>, Option<SuperParameter>) {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		Ok((this_annotation, super_annotation))
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		self.0.as_ref()
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		self.1.as_ref()
	}
}

impl<L, V> ASTNode for FunctionParameters<L, V>
where
	L: LeadingParameter,
	V: ParameterVisibility,
{
	fn get_position(&self) -> Span {
		self.position
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
		// TODO parameters don't implement ASTNode
		// let large = are_nodes_over_length(
		// 	parameters.iter(),
		// 	options,
		// 	local,
		// 	Some(MAX_INLINE_OBJECT_LITERAL),
		// 	true,
		// );
		buf.push('(');
		for (at_end, Parameter { name, type_annotation, additionally, .. }) in
			parameters.iter().endiate()
		{
			// decorators_to_string_from_buffer(decorators, buf, options, local);
			name.to_string_from_buffer(buf, options, local);
			if let (true, Some(ref type_annotation)) =
				(options.include_type_annotations, type_annotation)
			{
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

impl<L, V> FunctionParameters<L, V>
where
	L: LeadingParameter,
	V: ParameterVisibility,
{
	pub(crate) fn from_reader_sub_open_parenthesis(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut parameters = Vec::new();

		let mut this_type = None::<ThisParameter>;
		let mut super_type = None::<SuperParameter>;
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
				let name = SpreadParameterName::from_reader(reader, state, options)?;
				let name_position = name.get_position();

				let type_annotation = if options.type_annotations
					&& reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some()
				{
					Some(TypeAnnotation::from_reader(reader, state, options)?)
				} else {
					None
				};

				let position = spread_pos
					.union(type_annotation.as_ref().map_or(name_position, ASTNode::get_position));

				rest_parameter =
					Some(Box::new(SpreadParameter { name, type_annotation, position }));
				break;
			} else if let Some(Token(_, start)) = reader.conditional_next(|tok| {
				options.type_annotations
					&& parameters.is_empty()
					&& matches!(tok, TSXToken::Keyword(TSXKeyword::This))
			}) {
				reader.expect_next(TSXToken::Colon)?;
				let constraint = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(constraint.get_position());
				this_type = Some(ThisParameter { constraint, position });
			} else if let Some(Token(_, start)) = reader.conditional_next(|tok| {
				options.type_annotations
					&& parameters.is_empty()
					&& matches!(tok, TSXToken::Keyword(TSXKeyword::Super))
			}) {
				reader.expect_next(TSXToken::Colon)?;
				let constraint = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(constraint.get_position());
				super_type = Some(SuperParameter { constraint, position });
			} else {
				let visibility = V::from_reader(reader, state, options);

				let name = WithComment::<VariableField>::from_reader(reader, state, options)?;

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
					visibility,
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

		let leading = L::try_make(this_type, super_type)?;

		Ok(FunctionParameters { position: start.union(close), parameters, rest_parameter, leading })
	}
}

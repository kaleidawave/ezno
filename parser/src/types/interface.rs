use crate::{
	derive_ASTNode, errors::parse_lexing_error, extensions::decorators::Decorated,
	functions::MethodHeader, parse_bracketed, property_key::PublicOrPrivate,
	throw_unexpected_token_with_token, to_string_bracketed, tokens::token_as_identifier,
	types::type_annotations::TypeAnnotationFunctionParameters, ASTNode, Expression,
	ExpressionOrStatementPosition, NumberRepresentation, ParseErrors, ParseOptions, ParseResult,
	PropertyKey, Span, StatementPosition, TSXKeyword, TSXToken, TypeAnnotation, TypeParameter,
	WithComment,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::{sized_tokens::TokenReaderWithTokenEnds, Token, TokenReader};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct InterfaceDeclaration {
	pub is_declare: bool,
	pub name: StatementPosition,
	#[cfg(feature = "extras")]
	pub is_nominal: bool,
	pub type_parameters: Option<Vec<TypeParameter>>,
	/// The document interface extends a multiple of other interfaces
	pub extends: Option<Vec<TypeAnnotation>>,
	pub members: Vec<WithComment<Decorated<InterfaceMember>>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum Optionality {
	Default,
	Optional,
	// Will make existing optional fields required, whereas default does not change status
	Required,
}

// Used around type aliases for inline rule thingies
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum TypeRule {
	In,
	InKeyOf,
}

impl ASTNode for InterfaceDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::Interface)?;

		#[cfg(feature = "extras")]
		let is_nominal = reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Nominal)))
			.is_some();

		let name = StatementPosition::from_reader(reader, state, options)?;
		let type_parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				crate::parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
					.map(|(params, _)| params)
			})
			.transpose()?;

		let extends = if reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Extends)))
			.is_some()
		{
			let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
			let mut extends = vec![type_annotation];
			if reader.conditional_next(|t| matches!(t, TSXToken::Comma)).is_some() {
				loop {
					extends.push(TypeAnnotation::from_reader(reader, state, options)?);
					match reader.peek() {
						Some(Token(TSXToken::Comma, _)) => {
							reader.next();
						}
						Some(Token(TSXToken::OpenBrace, _)) | None => break,
						_ => {
							return throw_unexpected_token_with_token(
								reader.next().unwrap(),
								&[TSXToken::Comma, TSXToken::OpenBrace],
							)
						}
					}
				}
			}
			Some(extends)
		} else {
			None
		};

		reader.expect_next(TSXToken::OpenBrace)?;
		let members = parse_interface_members(reader, state, options)?;
		let position = start.union(reader.expect_next_get_end(TSXToken::CloseBrace)?);
		Ok(InterfaceDeclaration {
			name,
			is_declare: false,
			#[cfg(feature = "extras")]
			is_nominal,
			type_parameters,
			extends,
			members,
			position,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			if self.name.is_declare() {
				buf.push_str("declare ");
			}
			buf.push_str("interface ");
			self.name.identifier.to_string_from_buffer(buf, options, local);
			if let Some(type_parameters) = &self.type_parameters {
				to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				options.push_gap_optionally(buf);
			}
			if let Some(extends) = &self.extends {
				buf.push_str(" extends ");
				for (at_end, extends) in extends.iter().endiate() {
					extends.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
			}
			options.push_gap_optionally(buf);
			buf.push('{');
			if options.pretty && !self.members.is_empty() {
				buf.push_new_line();
			}
			for member in &self.members {
				options.add_indent(local.depth + 1, buf);
				member.to_string_from_buffer(buf, options, local.next_level());
				if options.pretty {
					buf.push_new_line();
				}
			}
			buf.push('}');
		}
	}

	fn get_position(&self) -> Span {
		self.position
	}
}

/// For some reason mapped types can have a negated a readonly keyword
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MappedReadonlyKind {
	Negated,
	Always,
	False,
}

/// This is also used for [`TypeAnnotation::ObjectLiteral`]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum InterfaceMember {
	Method {
		header: MethodHeader,
		name: PropertyKey<PublicOrPrivate>,
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_optional: bool,
		position: Span,
	},
	Property {
		name: PropertyKey<PublicOrPrivate>,
		type_annotation: TypeAnnotation,
		is_readonly: bool,
		/// Marked with `?:`
		is_optional: bool,
		position: Span,
	},
	Indexer {
		name: String,
		indexer_type: TypeAnnotation,
		return_type: TypeAnnotation,
		is_readonly: bool,
		position: Span,
	},
	/// Example
	/// ```ts
	/// new (...params: any[]): HTMLElement
	/// ```
	Constructor {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	Caller {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	/// [For mapped types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)
	Rule {
		parameter: String,
		rule: TypeRule,
		matching_type: Box<TypeAnnotation>,
		as_type: Option<Box<TypeAnnotation>>,
		optionality: Optionality,
		is_readonly: MappedReadonlyKind,
		output_type: Box<TypeAnnotation>,
		position: Span,
	},
	Comment(String, bool, Span),
}

#[allow(clippy::similar_names)]
impl ASTNode for InterfaceMember {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let readonly_position = state.optionally_expect_keyword(reader, TSXKeyword::Readonly);

		// This match will early return if not a method
		let token = &reader.peek().ok_or_else(parse_lexing_error)?.0;
		match token {
			// Calling self
			TSXToken::OpenParentheses => {
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;
				// let parameters = function_parameters_from_reader(reader, state, options)?;
				let return_type =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						Some(TypeAnnotation::from_reader(reader, state, options)?)
					} else {
						None
					};
				// TODO parameter.pos can be union'ed with itself
				let position = readonly_position
					.as_ref()
					.unwrap_or(&parameters.position)
					.union(return_type.as_ref().map_or(parameters.position, ASTNode::get_position));
				Ok(InterfaceMember::Caller {
					is_readonly: readonly_position.is_some(),
					position,
					parameters,
					return_type,
					type_parameters: None,
				})
			}
			// Caller self with generic parameters
			TSXToken::OpenChevron => {
				let (type_parameters, _start_pos) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)?;
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;
				let return_type =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						Some(TypeAnnotation::from_reader(reader, state, options)?)
					} else {
						None
					};
				let position =
					return_type.as_ref().map_or(parameters.position, ASTNode::get_position);

				Ok(InterfaceMember::Caller {
					is_readonly: readonly_position.is_some(),
					position,
					parameters,
					type_parameters: Some(type_parameters),
					return_type,
				})
			}
			// Constructor
			TSXToken::Keyword(TSXKeyword::New) => {
				let new_span = reader.next().unwrap().get_span();
				let type_parameters = reader
					.conditional_next(|token| *token == TSXToken::OpenChevron)
					.is_some()
					.then(|| parse_bracketed(reader, state, options, None, TSXToken::CloseChevron))
					.transpose()?
					.map(|(tp, _)| tp);

				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;

				let return_type = if reader
					.conditional_next(|tok| {
						options.type_annotations && matches!(tok, TSXToken::Colon)
					})
					.is_some()
				{
					Some(TypeAnnotation::from_reader(reader, state, options)?)
				} else {
					None
				};

				let end = return_type.as_ref().map_or(parameters.position, ASTNode::get_position);

				let position = readonly_position.as_ref().unwrap_or(&new_span).union(end);

				Ok(InterfaceMember::Constructor {
					is_readonly: readonly_position.is_some(),
					position,
					parameters,
					type_parameters,
					return_type,
				})
			}
			TSXToken::Subtract => {
				// Little bit weird, but prevents a lot of duplication
				let subtract_pos = reader.next().unwrap().get_span();
				let inner = Self::from_reader(reader, state, options)?;
				if let Self::Rule {
					parameter,
					rule,
					matching_type,
					as_type,
					optionality,
					is_readonly: MappedReadonlyKind::Always,
					output_type,
					position,
				} = inner
				{
					Ok(Self::Rule {
						parameter,
						rule,
						matching_type,
						as_type,
						optionality,
						is_readonly: MappedReadonlyKind::Negated,
						output_type,
						position,
					})
				} else {
					Err(crate::ParseError::new(ParseErrors::ExpectRule, subtract_pos))
				}
			}
			token if token.is_comment() => {
				let token = reader.next().unwrap();
				if let Ok((comment, is_multiline, span)) = TSXToken::try_into_comment(token) {
					Ok(InterfaceMember::Comment(comment, is_multiline, span))
				} else {
					unreachable!()
				}
			}
			_ => {
				let header = MethodHeader::from_reader(reader);

				// TODO tidy
				let (name, type_parameters) = if let TSXToken::OpenBracket =
					reader.peek().unwrap().0
				{
					// Non literal property names and index type
					let Token(_, start) = reader.next().unwrap();
					let name = match reader.next().ok_or_else(parse_lexing_error)? {
						Token(TSXToken::StringLiteral(name, quoted), start) => {
							let position = start.with_length(name.len() + 2);
							PropertyKey::StringLiteral(name, quoted, position)
						}
						Token(TSXToken::NumberLiteral(value), start) => {
							let position = start.with_length(value.len());
							PropertyKey::NumberLiteral(
								value.parse::<NumberRepresentation>().unwrap(),
								position,
							)
						}
						token => {
							// "name" is the name of the parameter name for indexing
							let (name, name_span) =
								token_as_identifier(token, "interface parameter")?;

							// Catch for computed symbol: e.g. `[Symbol.instanceOf()]`, rather than indexer
							if let Some(Token(TSXToken::Dot, _)) = reader.peek() {
								let top = Expression::VariableReference(name, name_span);
								let expression = Expression::from_reader_sub_first_expression(
									reader, state, options, 0, top,
								)?;
								let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
								PropertyKey::Computed(Box::new(expression), start.union(end))
							} else {
								let start_span = readonly_position.as_ref().unwrap_or(&name_span);
								match reader.next().ok_or_else(parse_lexing_error)? {
									// Indexed type
									Token(TSXToken::Colon, _) => {
										let indexer_type =
											TypeAnnotation::from_reader(reader, state, options)?;
										reader.expect_next(TSXToken::CloseBracket)?;
										reader.expect_next(TSXToken::Colon)?;
										let return_type =
											TypeAnnotation::from_reader(reader, state, options)?;
										return Ok(InterfaceMember::Indexer {
											name,
											is_readonly: readonly_position.is_some(),
											indexer_type,
											position: start_span.union(return_type.get_position()),
											return_type,
										});
									}
									Token(TSXToken::Keyword(TSXKeyword::In), _) => {
										let rule = if reader
											.conditional_next(|token| {
												matches!(
													token,
													TSXToken::Keyword(TSXKeyword::KeyOf)
												)
											})
											.is_some()
										{
											TypeRule::InKeyOf
										} else {
											TypeRule::In
										};

										let matching_type =
											TypeAnnotation::from_reader(reader, state, options)?;

										let next_is_as = reader.conditional_next(|t| {
											matches!(t, TSXToken::Keyword(TSXKeyword::As))
										});

										let as_type = if next_is_as.is_some() {
											Some(Box::new(TypeAnnotation::from_reader_with_config(
												reader, state, options, None, None,
											)?))
										} else {
											None
										};

										reader.expect_next(TSXToken::CloseBracket)?;
										// TODO the -?: ?: : stuff '-?:' should be a token
										let token = reader.next().ok_or_else(parse_lexing_error)?;
										let optionality = match token {
											Token(TSXToken::Colon, _) => Optionality::Default,
											Token(TSXToken::OptionalMember, _) => {
												Optionality::Optional
											}
											Token(TSXToken::NonOptionalMember, _) => {
												Optionality::Required
											}
											token => {
												return throw_unexpected_token_with_token(
													token,
													&[
														TSXToken::Colon,
														TSXToken::OptionalMember,
														TSXToken::NonOptionalMember,
													],
												);
											}
										};

										let output_type =
											TypeAnnotation::from_reader(reader, state, options)?;

										let position = start_span.union(output_type.get_position());

										return Ok(InterfaceMember::Rule {
											parameter: name,
											optionality,
											is_readonly: match readonly_position.is_some() {
												true => MappedReadonlyKind::Always,
												false => MappedReadonlyKind::False,
											},
											matching_type: Box::new(matching_type),
											rule,
											output_type: Box::new(output_type),
											position,
											as_type,
										});
									}
									token => {
										return throw_unexpected_token_with_token(
											token,
											&[TSXToken::Colon, TSXToken::Keyword(TSXKeyword::In)],
										);
									}
								}
							}
						}
					};
					(name, None)
				} else {
					let property_key = PropertyKey::from_reader(reader, state, options)?;
					let type_parameters = reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
						.then(|| {
							parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
						})
						.transpose()?;

					(property_key, type_parameters.map(|(tp, _)| tp))
				};

				let start = readonly_position.unwrap_or_else(|| name.get_position());

				// TODO a little weird as only functions can have type parameters:
				match reader.next().ok_or_else(parse_lexing_error)? {
					Token(TSXToken::OpenParentheses, _start_pos) => {
						let parameters =
							TypeAnnotationFunctionParameters::from_reader_sub_open_parenthesis(
								reader,
								state,
								options,
								start.get_start(),
							)?;
						let mut position = start.union(parameters.position);
						let return_type = if reader
							.conditional_next(|tok| matches!(tok, TSXToken::Colon))
							.is_some()
						{
							let type_annotation =
								TypeAnnotation::from_reader(reader, state, options)?;
							position = position.union(type_annotation.get_position());
							Some(type_annotation)
						} else {
							None
						};

						Ok(InterfaceMember::Method {
							header,
							name,
							parameters,
							type_parameters,
							return_type,
							is_optional: false,
							position,
						})
					}
					Token(TSXToken::QuestionMark, _) => {
						// TODO this is a little weird, I don't think '?(' is a actual token and is
						// only used here. Making '?(' a token may break ternary where first expr is a group
						let parameters =
							TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;

						let mut position = start.union(parameters.position);

						let return_type = if reader
							.conditional_next(|tok| matches!(tok, TSXToken::Colon))
							.is_some()
						{
							let type_annotation =
								TypeAnnotation::from_reader(reader, state, options)?;
							position = position.union(type_annotation.get_position());
							Some(type_annotation)
						} else {
							None
						};

						Ok(InterfaceMember::Method {
							header,
							name,
							parameters,
							type_parameters,
							is_optional: true,
							position,
							return_type,
						})
					}
					Token(TSXToken::Colon, _) => {
						let mut type_annotation =
							TypeAnnotation::from_reader(reader, state, options)?;

						if readonly_position.is_some() {
							// TODO positioning:
							let position = start.union(type_annotation.get_position());
							type_annotation =
								TypeAnnotation::Readonly(Box::new(type_annotation), position);
						}
						let position = start.union(type_annotation.get_position());
						Ok(InterfaceMember::Property {
							position,
							name,
							type_annotation,
							is_optional: false,
							is_readonly: readonly_position.is_some(),
						})
					}
					Token(TSXToken::OptionalMember, _) => {
						let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
						let position = start.union(type_annotation.get_position());
						Ok(InterfaceMember::Property {
							name,
							type_annotation,
							is_optional: true,
							is_readonly: readonly_position.is_some(),
							position,
						})
					}
					token => throw_unexpected_token_with_token(
						token,
						&[
							TSXToken::OpenParentheses,
							TSXToken::QuestionMark,
							TSXToken::Colon,
							TSXToken::OptionalMember,
						],
					),
				}
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			InterfaceMember::Property { name, type_annotation, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				name.to_string_from_buffer(buf, options, local);
				buf.push(':');
				options.push_gap_optionally(buf);
				type_annotation.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Method {
				name,
				type_parameters,
				parameters,
				return_type,
				is_optional,
				..
			} => {
				name.to_string_from_buffer(buf, options, local);
				if *is_optional {
					buf.push('?');
				}
				if let Some(type_parameters) = &type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Indexer { name, indexer_type, return_type, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				buf.push('[');
				buf.push_str(name.as_str());
				buf.push(':');
				indexer_type.to_string_from_buffer(buf, options, local);
				buf.push_str("]: ");
				return_type.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Constructor {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position: _,
			} => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				buf.push_str("new ");
				if let Some(ref type_parameters) = type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(ref return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Caller {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position: _,
			} => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				if let Some(ref type_parameters) = type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(ref return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Rule {
				is_readonly,
				matching_type,
				optionality,
				output_type,
				as_type,
				parameter,
				rule,
				position: _,
			} => {
				buf.push_str(match is_readonly {
					MappedReadonlyKind::Negated => "-readonly ",
					MappedReadonlyKind::Always => "readonly ",
					MappedReadonlyKind::False => "",
				});
				buf.push('[');
				buf.push_str(parameter.as_str());
				buf.push_str(match rule {
					TypeRule::In => " in ",
					TypeRule::InKeyOf => " in keyof ",
				});
				matching_type.to_string_from_buffer(buf, options, local);
				if let Some(as_type) = as_type {
					buf.push(' ');
					as_type.to_string_from_buffer(buf, options, local);
				}
				buf.push(']');
				buf.push_str(match optionality {
					Optionality::Default => ": ",
					Optionality::Optional => "?:",
					Optionality::Required => "-?:",
				});
				output_type.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Comment(_, _is_multiline, _) => todo!(),
		}
	}

	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}
}

pub(crate) fn parse_interface_members(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
) -> ParseResult<Vec<WithComment<Decorated<InterfaceMember>>>> {
	let mut members = Vec::new();
	loop {
		if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
			break;
		}
		let decorated_member = WithComment::from_reader(reader, state, options)?;
		members.push(decorated_member);
		// Semi colons and commas are optional here
		if let Some(Token(TSXToken::SemiColon | TSXToken::Comma, _)) = reader.peek() {
			reader.next();
		}
	}
	Ok(members)
}

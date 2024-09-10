use crate::{
	derive_ASTNode, extensions::decorators::Decorated, functions::MethodHeader, parse_bracketed,
	property_key::PublicOrPrivate, to_string_bracketed,
	types::type_annotations::TypeAnnotationFunctionParameters, ASTNode,
	ExpressionOrStatementPosition, ParseErrors, ParseOptions, ParseResult, PropertyKey, Span,
	StatementPosition, TypeAnnotation, TypeParameter, WithComment,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct InterfaceDeclaration {
	pub is_is_declare: bool,
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

impl ASTNode for InterfaceDeclaration {
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"let start = state.expect_keyword(reader, TSXKeyword::Interface)?;

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
					.map(|(params, _, _)| params)
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
			is_is_declare: false,
			#[cfg(feature = "extras")]
			is_nominal,
			type_parameters,
			extends,
			members,
			position,
		})"#;
		todo!();
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
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let _existing = r#"let readonly_position = state.optionally_expect_keyword(reader, TSXKeyword::Readonly);

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
				let _ = reader.next();
				let (type_parameters, _, _start_pos) =
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
					.map(|(tp, _, _)| tp);

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
				let first = reader
					.conditional_next(|t| matches!(t, TSXToken::OpenBracket))
					.map(|res| (None, res))
					.or_else(|| {
						let is_get_set_async_index_type =
							matches!(
								reader.peek(),
								Some(Token(
									TSXToken::Keyword(
										TSXKeyword::Get | TSXKeyword::Set | TSXKeyword::Async
									),
									_
								))
							) && matches!(reader.peek_n(1), Some(Token(TSXToken::OpenBracket, _)));

						if is_get_set_async_index_type {
							let token = reader.next().unwrap();
							let header = match token.0 {
								TSXToken::Keyword(TSXKeyword::Get) => MethodHeader::Get,
								TSXToken::Keyword(TSXKeyword::Set) => MethodHeader::Set,
								TSXToken::Keyword(TSXKeyword::Async) => {
									MethodHeader::Regular { is_async: true, generator: None }
								}
								_ => unreachable!(),
							};
							let open_bracket_token = reader.next().unwrap();
							Some((Some(header), open_bracket_token))
						} else {
							None
						}
					});

				// Non literal property names and index type
				let (header, name, type_parameters) = if let Some((header, Token(_, start))) = first
				{
					let name = match reader.next().ok_or_else(parse_lexing_error)? {
						Token(TSXToken::StringLiteral(name, quoted), start) => {
							let position = start.with_length(name.len() + 2);
							let _end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
							PropertyKey::StringLiteral(name, quoted, position)
						}
						Token(TSXToken::NumberLiteral(value), start) => {
							let position = start.with_length(value.len());
							let _end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
							PropertyKey::NumberLiteral(
								value.parse::<crate::number::NumberRepresentation>().unwrap(),
								position,
							)
						}
						token => {
							use crate::Expression;
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
									Token(TSXToken::Colon, _start) => {
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
									// For mapped types
									Token(TSXToken::Keyword(TSXKeyword::In), _) => {
										let matching_type =
											TypeAnnotation::from_reader(reader, state, options)?;

										let next_is_as = reader.conditional_next(|t| {
											matches!(t, TSXToken::Keyword(TSXKeyword::As))
										});

										let as_type = if next_is_as.is_some() {
											Some(Box::new(TypeAnnotation::from_reader(
												reader, state, options,
											)?))
										} else {
											None
										};

										reader.expect_next(TSXToken::CloseBracket)?;
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
											is_readonly: if readonly_position.is_some() {
												MappedReadonlyKind::Always
											} else {
												MappedReadonlyKind::False
											},
											matching_type: Box::new(matching_type),
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
					(header, name, None)
				} else {
					let (header, name) = crate::functions::get_method_name(reader, state, options)?;
					let type_parameters = reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
						.then(|| {
							parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
						})
						.transpose()?
						.map(|(tp, _, _)| tp);

					let name = name.get_ast();

					let header = if header.is_no_modifiers() { None } else { Some(header) };

					(header, name, type_parameters)
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
							header: header.unwrap_or_default(),
							name,
							parameters,
							type_parameters,
							return_type,
							is_optional: false,
							position,
						})
					}
					Token(TSXToken::QuestionMark, _) => {
						// This is a function. If it was a property it would be the `?:` token
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
							header: header.unwrap_or_default(),
							name,
							parameters,
							type_parameters,
							is_optional: true,
							position,
							return_type,
						})
					}
					t @ Token(TSXToken::Colon | TSXToken::OptionalMember, _) => {
						if header.is_none() {
							let type_annotation =
								TypeAnnotation::from_reader(reader, state, options)?;
							let position = start.union(type_annotation.get_position());
							let is_optional = matches!(t, Token(TSXToken::OptionalMember, _));
							Ok(InterfaceMember::Property {
								position,
								name,
								type_annotation,
								is_optional,
								is_readonly: readonly_position.is_some(),
							})
						} else {
							throw_unexpected_token_with_token(t, &[TSXToken::OpenParentheses])
						}
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
				position: _,
			} => {
				buf.push_str(match is_readonly {
					MappedReadonlyKind::Negated => "-readonly ",
					MappedReadonlyKind::Always => "readonly ",
					MappedReadonlyKind::False => "",
				});
				buf.push('[');
				buf.push_str(parameter.as_str());
				buf.push_str(" in ");
				matching_type.to_string_from_buffer(buf, options, local);
				if let Some(as_type) = as_type {
					buf.push_str(" as ");
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
			InterfaceMember::Comment(content, is_multiline, _) => {
				if *is_multiline {
					buf.push_str("/*");
					buf.push_str(content);
					buf.push_str("*/");
				} else {
					buf.push_str("//");
					buf.push_str(content);
					buf.push_new_line();
				}
			}
		}
	}

	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}
}

pub(crate) fn interface_members_from_reader(
	reader: &mut crate::new::Lexer,
) -> ParseResult<Vec<WithComment<Decorated<InterfaceMember>>>> {
	todo!()
	// let mut members = Vec::new();
	// loop {
	// 	if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
	// 		break;
	// 	}
	// 	let decorated_member = WithComment::from_reader(reader, state, options)?;
	// 	// Semi colons and commas are optional here. Should expect_semi_colon
	// 	if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
	// 		reader.next();
	// 	} else {
	// 		let _ = crate::expect_semi_colon(
	// 			reader,
	// 			&state.line_starts,
	// 			decorated_member.get_position().end,
	// 			options,
	// 		)?;
	// 	}
	// 	members.push(decorated_member);
	// }
	// Ok(members)
}

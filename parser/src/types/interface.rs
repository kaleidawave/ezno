use crate::{
	errors::parse_lexing_error,
	extensions::decorators::Decorated,
	parse_bracketed,
	property_key::PublicOrPrivate,
	throw_unexpected_token_with_token, to_string_bracketed,
	tokens::token_as_identifier,
	tsx_keywords,
	types::{type_annotations::TypeAnnotationFunctionParameters, type_declarations::*},
	ASTNode, Expression, GenericTypeConstraint, Keyword, MethodHeader, NumberRepresentation,
	ParseOptions, ParseResult, PropertyKey, Span, TSXKeyword, TSXToken, TypeAnnotation,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::{sized_tokens::TokenReaderWithTokenEnds, Token, TokenReader};

#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct InterfaceDeclaration {
	pub name: String,
	#[cfg(feature = "extras")]
	pub nominal_keyword: Option<Keyword<tsx_keywords::Nominal>>,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	/// The document interface extends a multiple of other interfaces
	pub extends: Option<Vec<TypeAnnotation>>,
	pub members: Vec<Decorated<InterfaceMember>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum Optionality {
	Default,
	Optional,
	// Will make existing optional fields required, whereas default does not change status
	Required,
}

// Used around type aliases for inline rule thingies
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum TypeRule {
	In,
	InKeyOf,
}

impl ASTNode for InterfaceDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Interface))?;

		#[cfg(feature = "extras")]
		let nominal_keyword = Keyword::optionally_from_reader(reader);
		// if let Some(Token(TSXToken::Keyword(TSXKeyword::Nominal), _)) = reader.peek() {
		// 	Some((reader.next().unwrap().1))
		// } else {
		// 	None
		// };

		let TypeDeclaration { name, type_parameters, .. } =
			TypeDeclaration::from_reader(reader, state, settings)?;

		let extends = if let Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) = reader.peek()
		{
			reader.next();
			let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
			let mut extends = vec![type_annotation];
			if matches!(reader.peek(), Some(Token(TSXToken::Comma, _))) {
				reader.next();
				loop {
					extends.push(TypeAnnotation::from_reader(reader, state, settings)?);
					match reader.next().ok_or_else(parse_lexing_error)? {
						Token(TSXToken::Comma, _) => {
							reader.next();
						}
						Token(TSXToken::OpenBrace, _) => break,
						token => {
							return throw_unexpected_token_with_token(
								token,
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
		let members = parse_interface_members(reader, state, settings)?;
		let position = start.union(reader.expect_next_get_end(TSXToken::CloseBrace)?);
		Ok(InterfaceDeclaration {
			name,
			members,
			type_parameters,
			extends,
			position,
			#[cfg(feature = "extras")]
			nominal_keyword,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if settings.include_types {
			buf.push_str("interface ");
			buf.push_str(&self.name);
			if let Some(type_parameters) = &self.type_parameters {
				to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth);
			}
			settings.add_gap(buf);
			if let Some(extends) = &self.extends {
				buf.push_str(" extends ");
				for (at_end, extends) in extends.iter().endiate() {
					extends.to_string_from_buffer(buf, settings, depth);
					if !at_end {
						buf.push(',');
					}
				}
			}
			buf.push('{');
			if settings.pretty && !self.members.is_empty() {
				buf.push_new_line();
			}
			for member in self.members.iter() {
				settings.add_indent(depth + 1, buf);
				member.to_string_from_buffer(buf, settings, depth + 1);
				if settings.pretty {
					buf.push_new_line();
				}
			}
			buf.push('}');
		}
	}

	fn get_position(&self) -> &Span {
		&self.position
	}
}

/// This is also used for [TypeAnnotation::ObjectLiteral]
#[derive(Debug, Clone, PartialEq, Eq, GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum InterfaceMember {
	Method {
		kind: Option<MethodHeader>,
		name: PropertyKey<PublicOrPrivate>,
		type_parameters: Option<Vec<GenericTypeConstraint>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_optional: bool,
		#[cfg(feature = "extras")]
		performs: Option<super::AnnotationPerforms>,
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
		parameters: TypeAnnotationFunctionParameters,
		type_parameters: Option<Vec<GenericTypeConstraint>>,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	Caller {
		parameters: TypeAnnotationFunctionParameters,
		type_parameters: Option<Vec<GenericTypeConstraint>>,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	// Does exists on inline object reference
	Rule {
		parameter: String,
		rule: TypeRule,
		matching_type: Box<TypeAnnotation>,
		optionality: Optionality,
		is_readonly: bool,
		output_type: Box<TypeAnnotation>,
		position: Span,
	},
	Comment(String, Span),
}

impl ASTNode for InterfaceMember {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let readonly_keyword = Keyword::<tsx_keywords::Readonly>::optionally_from_reader(reader);

		let is_readonly = readonly_keyword.is_some();

		// This match will early return if not a method
		let token = &reader.peek().ok_or_else(parse_lexing_error)?.0;
		match token {
			// Calling self
			TSXToken::OpenParentheses => {
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;
				// let parameters = function_parameters_from_reader(reader, state, settings)?;
				let return_type = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
					reader.next();
					Some(TypeAnnotation::from_reader(reader, state, settings)?)
				} else {
					None
				};
				// TODO parameter.pos can be union'ed with itself
				let position = readonly_keyword
					.as_ref()
					.map_or(&parameters.position, |kw| kw.get_position())
					.union(
						return_type
							.as_ref()
							.map(ASTNode::get_position)
							.unwrap_or(&parameters.position),
					);
				Ok(InterfaceMember::Caller {
					is_readonly,
					position,
					parameters,
					return_type,
					type_parameters: None,
				})
			}
			// Caller self with generic parameters
			TSXToken::OpenChevron => {
				let (type_parameters, _start_pos) =
					parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?;
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;
				let return_type = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
					reader.next();
					Some(TypeAnnotation::from_reader(reader, state, settings)?)
				} else {
					None
				};
				let position = return_type
					.as_ref()
					.map_or(&parameters.position, ASTNode::get_position)
					.clone();

				Ok(InterfaceMember::Caller {
					is_readonly,
					position,
					parameters,
					type_parameters: Some(type_parameters),
					return_type,
				})
			}
			// Constructor
			TSXToken::Keyword(TSXKeyword::New) => {
				let new_span = reader.next().unwrap().get_span();
				let type_parameters = if reader
					.conditional_next(|token| *token == TSXToken::OpenChevron)
					.is_some()
				{
					Some(parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?.0)
				} else {
					None
				};

				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;

				let return_type =
					if reader.conditional_next(|token| *token == TSXToken::Colon).is_some() {
						Some(TypeAnnotation::from_reader(reader, state, settings)?)
					} else {
						None
					};

				Ok(InterfaceMember::Constructor {
					is_readonly,
					position: readonly_keyword
						.as_ref()
						.map_or(&new_span, |kw| kw.get_position())
						.union(
							return_type
								.as_ref()
								.map(ASTNode::get_position)
								.unwrap_or(&parameters.position),
						),
					parameters,
					type_parameters,
					return_type,
				})
			}
			TSXToken::MultiLineComment(..) | TSXToken::Comment(..) => {
				let token = reader.next().unwrap();
				let span = token.get_span();
				let comment = if let TSXToken::MultiLineComment(comment)
				| TSXToken::Comment(comment) = token.0
				{
					comment
				} else {
					unreachable!()
				};
				Ok(InterfaceMember::Comment(comment, span))
			}
			_ => {
				let kind = MethodHeader::optional_from_reader(reader);

				// TODO tidy
				let (name, type_parameters) = if let TSXToken::OpenBracket =
					reader.peek().unwrap().0
				{
					// Non literal property names and index type
					let Token(_, start) = reader.next().unwrap();
					let name = match reader.next().ok_or_else(parse_lexing_error)? {
						Token(TSXToken::SingleQuotedStringLiteral(name), start)
						| Token(TSXToken::DoubleQuotedStringLiteral(name), start) => {
							let position = start.with_length(name.len() + 2);
							PropertyKey::StringLiteral(name, position)
						}
						Token(TSXToken::NumberLiteral(value), start) => {
							let position = start.with_length(value.len());
							PropertyKey::NumberLiteral(
								value.parse::<NumberRepresentation>().unwrap(),
								position.clone(),
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
									reader, state, settings, 0, top,
								)?;
								let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
								PropertyKey::Computed(Box::new(expression), start.union(end))
							} else {
								let start_span = readonly_keyword
									.as_ref()
									.map_or(&name_span, |kw| kw.get_position());
								match reader.next().ok_or_else(parse_lexing_error)? {
									// Indexed type
									Token(TSXToken::Colon, _) => {
										let indexer_type =
											TypeAnnotation::from_reader(reader, state, settings)?;
										reader.expect_next(TSXToken::CloseBracket)?;
										reader.expect_next(TSXToken::Colon)?;
										let return_type =
											TypeAnnotation::from_reader(reader, state, settings)?;
										return Ok(InterfaceMember::Indexer {
											name,
											indexer_type,
											position: start_span.union(return_type.get_position()),
											return_type,
											is_readonly,
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
											TypeAnnotation::from_reader(reader, state, settings)?;
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
											TypeAnnotation::from_reader(reader, state, settings)?;

										let position = start_span.union(output_type.get_position());

										return Ok(InterfaceMember::Rule {
											parameter: name,
											optionality,
											is_readonly,
											matching_type: Box::new(matching_type),
											rule,
											output_type: Box::new(output_type),
											position,
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
					let property_key = PropertyKey::from_reader(reader, state, settings)?;
					let type_parameters = reader
						.conditional_next(|token| *token == TSXToken::OpenChevron)
						.is_some()
						.then(|| {
							parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)
						})
						.transpose()?;

					if let Some((type_parameters, _last_chevron)) = type_parameters {
						(property_key, Some(type_parameters))
					} else {
						(property_key, None)
					}
				};

				let start = readonly_keyword.as_ref().map_or_else(
					|| name.get_position().get_start(),
					|kw| kw.get_position().get_start(),
				);

				// TODO a little weird as only functions can have type parameters:
				match reader.next().ok_or_else(parse_lexing_error)? {
					Token(TSXToken::OpenParentheses, _start_pos) => {
						let parameters =
							TypeAnnotationFunctionParameters::from_reader_sub_open_parenthesis(
								reader, state, settings, start,
							)?;
						let mut position = start.union(&parameters.position);
						let return_type = if reader
							.conditional_next(|tok| matches!(tok, TSXToken::Colon))
							.is_some()
						{
							let type_annotation =
								TypeAnnotation::from_reader(reader, state, settings)?;
							position = position.union(type_annotation.get_position());
							Some(type_annotation)
						} else {
							None
						};

						#[cfg(feature = "extras")]
						let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) =
							reader.peek()
						{
							Some(super::AnnotationPerforms::from_reader(reader, state, settings)?)
						} else {
							None
						};

						Ok(InterfaceMember::Method {
							kind,
							name,
							parameters,
							type_parameters,
							return_type,
							is_optional: false,
							position,
							#[cfg(feature = "extras")]
							performs,
						})
					}
					Token(TSXToken::QuestionMark, _) => {
						// TODO this is a little weird, I don't think '?(' is a actual token and is
						// only used here. Making '?(' a token may break ternary where first expr is a group
						let parameters =
							TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;

						let mut position = start.union(&parameters.position);

						let return_type = if reader
							.conditional_next(|tok| matches!(tok, TSXToken::Colon))
							.is_some()
						{
							let type_annotation =
								TypeAnnotation::from_reader(reader, state, settings)?;
							position = position.union(type_annotation.get_position());
							Some(type_annotation)
						} else {
							None
						};

						#[cfg(feature = "extras")]
						let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) =
							reader.peek()
						{
							Some(super::AnnotationPerforms::from_reader(reader, state, settings)?)
						} else {
							None
						};

						Ok(InterfaceMember::Method {
							kind,
							name,
							parameters,
							type_parameters,
							is_optional: true,
							position,
							return_type,
							#[cfg(feature = "extras")]
							performs,
						})
					}
					Token(TSXToken::Colon, _) => {
						let mut type_annotation =
							TypeAnnotation::from_reader(reader, state, settings)?;
						if is_readonly {
							// TODO positioning:
							let position = start.union(type_annotation.get_position());
							type_annotation =
								TypeAnnotation::Readonly(Box::new(type_annotation), position);
						}
						Ok(InterfaceMember::Property {
							name,
							position: start.union(type_annotation.get_position()),
							type_annotation,
							is_optional: false,
							is_readonly,
						})
					}
					Token(TSXToken::OptionalMember, _) => {
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						let position = start.union(type_annotation.get_position());
						Ok(InterfaceMember::Property {
							name,
							type_annotation,
							is_optional: true,
							is_readonly,
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
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Property { name, type_annotation, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				name.to_string_from_buffer(buf, settings, depth);
				buf.push(':');
				settings.add_gap(buf);
				type_annotation.to_string_from_buffer(buf, settings, depth);
			}
			Self::Method {
				name, type_parameters, parameters, return_type, is_optional, ..
			} => {
				name.to_string_from_buffer(buf, settings, depth);
				if *is_optional {
					buf.push('?');
				}
				if let Some(type_parameters) = &type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth);
				}
				parameters.to_string_from_buffer(buf, settings, depth);
				if let Some(return_type) = return_type {
					buf.push(':');
					settings.add_gap(buf);
					return_type.to_string_from_buffer(buf, settings, depth);
				}
			}
			Self::Indexer { name, indexer_type, return_type, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				buf.push('[');
				buf.push_str(name.as_str());
				buf.push(':');
				indexer_type.to_string_from_buffer(buf, settings, depth);
				buf.push(']');
				buf.push(':');
				settings.add_gap(buf);
				return_type.to_string_from_buffer(buf, settings, depth);
			}
			_ => todo!(),
		}
	}

	fn get_position(&self) -> &Span {
		GetFieldByType::get(self)
	}
}

pub(crate) fn parse_interface_members(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	settings: &ParseOptions,
) -> ParseResult<Vec<Decorated<InterfaceMember>>> {
	let mut members = Vec::new();
	loop {
		if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
			break;
		}
		let decorated_member = Decorated::<InterfaceMember>::from_reader(reader, state, settings)?;
		members.push(decorated_member);
		// Semi colons and commas are optional here
		if let Some(Token(TSXToken::SemiColon | TSXToken::Comma, _)) = reader.peek() {
			reader.next();
		}
	}
	Ok(members)
}

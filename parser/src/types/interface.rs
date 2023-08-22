use std::borrow::Cow;

use crate::{
	errors::parse_lexing_error,
	extensions::decorators::Decorated,
	parse_bracketed, to_string_bracketed,
	tokens::token_as_identifier,
	tsx_keywords,
	types::{type_annotations::TypeAnnotationFunctionParameters, type_declarations::*},
	ASTNode, Expression, GenericTypeConstraint, Keyword, NumberStructure, ParseError, ParseErrors,
	ParseOptions, ParseResult, PropertyKey, Span, TSXKeyword, TSXToken, TypeAnnotation,
};

use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::{Token, TokenReader};

use super::AnnotationPerforms;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
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
pub enum Optionality {
	Default,
	Optional,
	// Will make existing optional fields required, whereas default does not change status
	Required,
}

// Used around type aliases for inline rule thingies
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum TypeRule {
	In,
	InKeyOf,
}

impl ASTNode for InterfaceDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Interface))?;

		let nominal_keyword =
			if let Some(Token(TSXToken::Keyword(TSXKeyword::Nominal), _)) = reader.peek() {
				Some(Keyword::new(reader.next().unwrap().1))
			} else {
				None
			};

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
					match reader.peek() {
						Some(Token(TSXToken::Comma, _)) => {
							reader.next();
						}
						Some(Token(TSXToken::OpenBrace, _)) => break,
						_ => unimplemented!(),
					}
				}
			}
			Some(extends)
		} else {
			None
		};

		reader.expect_next(TSXToken::OpenBrace)?;
		let members = parse_interface_members(reader, state, settings)?;
		let position = start.union(&reader.expect_next(TSXToken::CloseBrace)?);
		Ok(InterfaceDeclaration {
			nominal_keyword,
			name,
			members,
			type_parameters,
			extends,
			position,
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

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}
}

/// This is also used for [TypeAnnotation::ObjectLiteral]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum InterfaceMember {
	Method {
		name: PropertyKey,
		type_parameters: Option<Vec<GenericTypeConstraint>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_optional: bool,
		#[cfg(feature = "extras")]
		performs: Option<AnnotationPerforms>,
		position: Span,
	},
	Property {
		name: PropertyKey,
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
	// Only exists on inline object reference
	Rule {
		parameter: String,
		rule: TypeRule,
		matching_type: Box<TypeAnnotation>,
		optionality: Optionality,
		is_readonly: bool,
		output_type: Box<TypeAnnotation>,
		position: Span,
	},
	Comment(String),
}

impl ASTNode for InterfaceMember {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let readonly_pos = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Readonly)))
			.map(|tok| tok.1);

		let is_readonly = readonly_pos.is_some();

		// TODO remove pos
		// This match will early return if not a method
		let (name, type_parameters, pos) = match &reader.peek().ok_or_else(parse_lexing_error)?.0 {
			// Non literal property names and index type
			TSXToken::OpenBracket => {
				let Token(_, start_pos) = reader.next().unwrap();
				let (name, end_pos) = match reader.next().ok_or_else(parse_lexing_error)? {
					Token(TSXToken::SingleQuotedStringLiteral(name), pos)
					| Token(TSXToken::DoubleQuotedStringLiteral(name), pos) => {
						(PropertyKey::StringLiteral(name, pos.clone()), pos)
					}
					Token(TSXToken::NumberLiteral(value), pos) => (
						PropertyKey::NumberLiteral(
							value.parse::<NumberStructure>().unwrap(),
							pos.clone(),
						),
						pos,
					),
					token => {
						// "name" is the name of the parameter name for indexing
						let (name, name_pos) = token_as_identifier(token, "interface parameter")?;

						// Catch for computed symbol: e.g. `[Symbol.instanceOf()]`, rather than indexer
						if let Some(Token(TSXToken::Dot, _)) = reader.peek() {
							let top = Expression::VariableReference(name, name_pos);
							// TODO bad
							let expression = Expression::from_reader_sub_first_expression(
								reader, state, settings, 0, top,
							)?;
							let end_pos = reader.expect_next(TSXToken::CloseBracket)?;
							(
								PropertyKey::Computed(
									Box::new(expression),
									start_pos.union(&end_pos),
								),
								end_pos,
							)
						} else {
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
										position: readonly_pos
											.unwrap_or(name_pos)
											.union(&return_type.get_position()),
										return_type,
										is_readonly,
									});
								}
								Token(TSXToken::Keyword(TSXKeyword::In), _) => {
									let rule = if matches!(
										reader.peek().unwrap().0,
										TSXToken::Keyword(TSXKeyword::KeyOf)
									) {
										reader.next();
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
										Token(TSXToken::OptionalMember, _) => Optionality::Optional,
										Token(TSXToken::NonOptionalMember, _) => {
											Optionality::Required
										}
										Token(token, position) => {
											return Err(ParseError::new(
												ParseErrors::UnexpectedToken {
													expected: &[
														TSXToken::Colon,
														TSXToken::OptionalMember,
														TSXToken::NonOptionalMember,
													],
													found: token,
												},
												position,
											));
										}
									};
									let output_type =
										TypeAnnotation::from_reader(reader, state, settings)?;
									let position = readonly_pos
										.unwrap_or(name_pos)
										.union(&output_type.get_position());
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
								Token(token, position) => {
									return Err(ParseError::new(
										crate::ParseErrors::UnexpectedToken {
											expected: &[
												TSXToken::Colon,
												TSXToken::Keyword(TSXKeyword::In),
											],
											found: token,
										},
										position,
									))
								}
							}
						}
					}
				};
				(name, None, end_pos)
			}
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
				let position = readonly_pos.unwrap_or(parameters.position.clone()).union(
					&return_type
						.as_ref()
						.map(ASTNode::get_position)
						.unwrap_or(Cow::Borrowed(&parameters.position)),
				);
				return Ok(InterfaceMember::Caller {
					is_readonly,
					position,
					parameters,
					return_type,
					type_parameters: None,
				});
			}
			// Caller self with generic parameters
			TSXToken::OpenChevron => {
				let (type_parameters, start_pos) =
					parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?;
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;
				let return_type = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
					reader.next();
					Some(TypeAnnotation::from_reader(reader, state, settings)?)
				} else {
					None
				};
				return Ok(InterfaceMember::Caller {
					is_readonly,
					position: readonly_pos.unwrap_or(start_pos).union(
						&return_type
							.as_ref()
							.map(ASTNode::get_position)
							.unwrap_or(Cow::Borrowed(&parameters.position)),
					),
					parameters,
					type_parameters: Some(type_parameters),
					return_type,
				});
			}
			// Constructor
			TSXToken::Keyword(TSXKeyword::New) => {
				let Token(_, new_span) = reader.next().unwrap();
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
				return Ok(InterfaceMember::Constructor {
					is_readonly,
					position: readonly_pos.unwrap_or(new_span).union(
						&return_type
							.as_ref()
							.map(ASTNode::get_position)
							.unwrap_or(Cow::Borrowed(&parameters.position)),
					),
					parameters,
					type_parameters,
					return_type,
				});
			}
			TSXToken::MultiLineComment(..) | TSXToken::Comment(..) => {
				let comment = if let TSXToken::MultiLineComment(comment)
				| TSXToken::Comment(comment) = reader.next().unwrap().0
				{
					comment
				} else {
					unreachable!()
				};
				return Ok(InterfaceMember::Comment(comment));
			}
			// yah weird
			TSXToken::SingleQuotedStringLiteral(..) | TSXToken::DoubleQuotedStringLiteral(..) => {
				let Token(token, position) = reader.next().unwrap();
				if let TSXToken::SingleQuotedStringLiteral(name)
				| TSXToken::DoubleQuotedStringLiteral(name) = token
				{
					(PropertyKey::StringLiteral(name, position.clone()), None, position)
				} else {
					unreachable!()
				}
			}
			_ => {
				let TypeDeclaration { name, type_parameters, position } =
					TypeDeclaration::from_reader(reader, state, settings)?;
				(PropertyKey::Ident(name, position.clone()), type_parameters, position)
			}
		};

		let mut position = readonly_pos.unwrap_or(pos);

		// TODO a little weird as only functions can have type parameters:
		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::OpenParentheses, start_pos) => {
				let parameters =
					TypeAnnotationFunctionParameters::from_reader_sub_open_parenthesis(
						reader, state, settings, start_pos,
					)?;
				position = position.union(&parameters.position);
				let return_type =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						position = position.union(&type_annotation.get_position());
						Some(type_annotation)
					} else {
						None
					};

				#[cfg(feature = "extras")]
				let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) = reader.peek() {
					Some(AnnotationPerforms::from_reader(reader, state, settings)?)
				} else {
					None
				};

				Ok(InterfaceMember::Method {
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

				position = position.union(&parameters.position);

				let return_type =
					if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some() {
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						position = position.union(&type_annotation.get_position());
						Some(type_annotation)
					} else {
						None
					};

				#[cfg(feature = "extras")]
				let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) = reader.peek() {
					Some(AnnotationPerforms::from_reader(reader, state, settings)?)
				} else {
					None
				};

				Ok(InterfaceMember::Method {
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
				let mut type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
				if is_readonly {
					// TODO positioning:
					position = position.union(&type_annotation.get_position());
					type_annotation =
						TypeAnnotation::Readonly(Box::new(type_annotation), position.clone());
				}
				Ok(InterfaceMember::Property {
					name,
					position,
					type_annotation,
					is_optional: false,
					is_readonly,
				})
			}
			Token(TSXToken::OptionalMember, _) => {
				let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
				let position = position.union(&type_annotation.get_position());
				Ok(InterfaceMember::Property {
					name,
					type_annotation,
					is_optional: true,
					is_readonly,
					position,
				})
			}
			Token(token, position) => Err(ParseError::new(
				crate::ParseErrors::UnexpectedToken {
					expected: &[
						TSXToken::OpenParentheses,
						TSXToken::QuestionMark,
						TSXToken::Colon,
						TSXToken::OptionalMember,
					],
					found: token,
				},
				position,
			)),
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
			_ => unimplemented!(),
		}
	}

	fn get_position(&self) -> Cow<Span> {
		match self {
			InterfaceMember::Method { position, .. }
			| InterfaceMember::Property { position, .. }
			| InterfaceMember::Indexer { position, .. }
			| InterfaceMember::Constructor { position, .. }
			| InterfaceMember::Caller { position, .. }
			| InterfaceMember::Rule { position, .. } => Cow::Borrowed(position),
			InterfaceMember::Comment(_) => todo!(),
		}
	}
}

pub(crate) fn parse_interface_members(
	reader: &mut impl TokenReader<TSXToken, Span>,
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

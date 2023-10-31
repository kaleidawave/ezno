use crate::{
	errors::parse_lexing_error, throw_unexpected_token, ASTNode, Expression, Keyword, ParseError,
	ParseOptions, ParseResult, Span, StatementPosition, TSXKeyword, TSXToken, Token,
	VariableIdentifier,
};

use super::{
	variable::VariableDeclaration, ClassDeclaration, ImportExportName, InterfaceDeclaration,
	StatementFunction, TypeAlias,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

/// TODO tidy up into struct
///
/// [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export)
#[derive(Debug, PartialEq, Eq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum ExportDeclaration {
	// TODO listed object thing
	// TODO export *
	Variable { exported: Exportable, position: Span },
	// `export default ...`
	Default { expression: Box<Expression>, position: Span },
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum Exportable {
	Class(ClassDeclaration<StatementPosition>),
	Function(StatementFunction),
	Variable(VariableDeclaration),
	Interface(InterfaceDeclaration),
	TypeAlias(TypeAlias),
	Parts(Vec<ExportPart>),
	ImportAll { r#as: Option<VariableIdentifier>, from: String },
	ImportParts { parts: Vec<ExportPart>, from: String },
}

impl ASTNode for ExportDeclaration {
	fn get_position(&self) -> &Span {
		match self {
			ExportDeclaration::Variable { position, .. }
			| ExportDeclaration::Default { position, .. } => position,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Export))?;
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Keyword(TSXKeyword::Default), _) => {
				reader.next();
				let expression = Expression::from_reader(reader, state, settings)?;
				let position = start.union(expression.get_position());
				Ok(ExportDeclaration::Default { expression: Box::new(expression), position })
			}
			Token(TSXToken::Multiply, _) => {
				reader.next();
				let r#as = if let Some(Token(TSXToken::Keyword(TSXKeyword::As), _)) = reader.peek()
				{
					reader.next();
					Some(VariableIdentifier::from_reader(reader, state, settings)?)
				} else {
					None
				};
				reader.expect_next(TSXToken::Keyword(TSXKeyword::From))?;
				let token = reader.next().ok_or_else(parse_lexing_error)?;
				let (end, from) = match token {
					Token(
						TSXToken::DoubleQuotedStringLiteral(from)
						| TSXToken::SingleQuotedStringLiteral(from),
						start,
					) => {
						let span = start.with_length(from.len() + 2);
						(span, from)
					}
					token => {
						let position = token.get_span();
						return Err(ParseError::new(
							crate::ParseErrors::ExpectedStringLiteral { found: token.0 },
							position,
						));
					}
				};
				Ok(ExportDeclaration::Variable {
					exported: Exportable::ImportAll { r#as, from },
					position: start.union(end),
				})
			}
			Token(TSXToken::Keyword(TSXKeyword::Class), _) => {
				let token = reader.next().unwrap();
				let keyword = Keyword::new(token.get_span());
				let class_declaration = ClassDeclaration::from_reader_sub_class_keyword(
					reader, state, settings, keyword,
				)?;
				let position = start.union(class_declaration.get_position());
				Ok(Self::Variable { exported: Exportable::Class(class_declaration), position })
			}
			Token(TSXToken::Keyword(TSXKeyword::Function), _) => {
				let function_declaration = StatementFunction::from_reader(reader, state, settings)?;
				let position = start.union(function_declaration.get_position());
				Ok(Self::Variable {
					exported: Exportable::Function(function_declaration),
					position,
				})
			}
			Token(TSXToken::Keyword(TSXKeyword::Const) | TSXToken::Keyword(TSXKeyword::Let), _) => {
				let variable_declaration =
					VariableDeclaration::from_reader(reader, state, settings)?;
				let position = start.union(variable_declaration.get_position());
				Ok(Self::Variable {
					exported: Exportable::Variable(variable_declaration),
					position,
				})
			}
			Token(TSXToken::Keyword(TSXKeyword::Interface), _) => {
				let interface_declaration =
					InterfaceDeclaration::from_reader(reader, state, settings)?;
				let position = start.union(interface_declaration.get_position());
				Ok(Self::Variable {
					exported: Exportable::Interface(interface_declaration),
					position,
				})
			}
			Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
				let type_alias = TypeAlias::from_reader(reader, state, settings)?;
				let position = start.union(type_alias.get_position());
				Ok(Self::Variable { exported: Exportable::TypeAlias(type_alias), position })
			}
			Token(TSXToken::OpenBrace, _) => {
				let Token(_, start) = reader.next().unwrap();
				let mut bracket_depth = 1;
				let after_bracket = reader.scan(|token, _| match token {
					TSXToken::OpenBrace => {
						bracket_depth += 1;
						false
					}
					TSXToken::CloseBrace => {
						bracket_depth -= 1;
						bracket_depth == 0
					}
					_ => false,
				});
				if let Some(Token(token_type, _)) = after_bracket {
					if let TSXToken::Keyword(TSXKeyword::From) = token_type {
						let (parts, _end) = crate::parse_bracketed::<ExportPart>(
							reader,
							state,
							settings,
							None,
							TSXToken::CloseBrace,
						)?;
						// Know this is 'from' from above
						let _ = reader.next().unwrap();
						let (end, from) = match reader.next().ok_or_else(parse_lexing_error)? {
							Token(
								TSXToken::DoubleQuotedStringLiteral(from)
								| TSXToken::SingleQuotedStringLiteral(from),
								start,
							) => {
								let span = start.with_length(from.len() + 2);
								(span, from)
							}
							token => {
								let position = token.get_span();
								return Err(ParseError::new(
									crate::ParseErrors::ExpectedStringLiteral { found: token.0 },
									position,
								));
							}
						};
						Ok(Self::Variable {
							exported: Exportable::ImportParts { parts, from },
							position: start.union(end),
						})
					} else {
						let (parts, end) = crate::parse_bracketed::<ExportPart>(
							reader,
							state,
							settings,
							None,
							TSXToken::CloseBrace,
						)?;
						Ok(Self::Variable {
							exported: Exportable::Parts(parts),
							position: start.union(end),
						})
					}
				} else {
					return Err(ParseError::new(
						crate::ParseErrors::UnmatchedBrackets,
						start.with_length(1),
					));
				}
			}
			_ => throw_unexpected_token(
				reader,
				&[
					TSXToken::Keyword(TSXKeyword::Class),
					TSXToken::Keyword(TSXKeyword::Function),
					TSXToken::Keyword(TSXKeyword::Const),
					TSXToken::Keyword(TSXKeyword::Let),
					TSXToken::Keyword(TSXKeyword::Interface),
					TSXToken::Keyword(TSXKeyword::Type),
					TSXToken::OpenBrace,
				],
			),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			ExportDeclaration::Variable { exported, .. } => {
				buf.push_str("export ");
				match exported {
					Exportable::Class(class_declaration) => {
						class_declaration.to_string_from_buffer(buf, settings, depth);
					}
					Exportable::Function(function_declaration) => {
						function_declaration.to_string_from_buffer(buf, settings, depth);
					}
					Exportable::Interface(interface_declaration) => {
						interface_declaration.to_string_from_buffer(buf, settings, depth);
					}
					Exportable::Variable(variable_dec_stmt) => {
						variable_dec_stmt.to_string_from_buffer(buf, settings, depth);
					}
					Exportable::TypeAlias(type_alias) => {
						type_alias.to_string_from_buffer(buf, settings, depth);
					}
					Exportable::Parts(parts) => {
						buf.push('{');
						settings.add_gap(buf);
						for (at_end, part) in parts.iter().endiate() {
							part.to_string_from_buffer(buf, settings, depth);
							if !at_end {
								buf.push(',');
								settings.add_gap(buf);
							}
						}
						settings.add_gap(buf);
						buf.push('}');
					}
					Exportable::ImportAll { r#as, from } => {
						buf.push_str("* ");
						if let Some(r#as) = r#as {
							buf.push_str("as ");
							r#as.to_string_from_buffer(buf, settings, depth);
							buf.push(' ');
						}
						buf.push_str("from \"");
						buf.push_str(from);
						buf.push('"');
					}
					Exportable::ImportParts { parts, from } => {
						buf.push('{');
						settings.add_gap(buf);
						for (at_end, part) in parts.iter().endiate() {
							part.to_string_from_buffer(buf, settings, depth);
							if !at_end {
								buf.push(',');
								settings.add_gap(buf);
							}
						}
						settings.add_gap(buf);
						buf.push('}');
						settings.add_gap(buf);
						buf.push_str("from \"");
						buf.push_str(from);
						buf.push('"');
					}
				}
			}
			ExportDeclaration::Default { expression, position: _ } => {
				buf.push_str("export default ");
				expression.to_string_from_buffer(buf, settings, depth);
			}
		}
	}
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import#syntax>
///
/// Similar to [ImportPart] but reversed
#[derive(Debug, Clone, PartialEq, Eq, Visitable, GetFieldByType)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
#[get_field_by_type_target(Span)]
pub enum ExportPart {
	Name(VariableIdentifier),
	NameWithAlias { name: String, alias: ImportExportName, position: Span },
	PrefixComment(String, Option<Box<Self>>, Span),
	PostfixComment(Box<Self>, String, Span),
}

impl ASTNode for ExportPart {
	fn get_position(&self) -> &Span {
		GetFieldByType::get(self)
	}

	// TODO also single line comments here
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		if let Token(TSXToken::MultiLineComment(comment), start) = token {
			let (position, under) =
				if let Some(Token(TSXToken::CloseBrace | TSXToken::Comma, _)) = reader.peek() {
					(start.with_length(comment.len() + 2), None)
				} else {
					let part = Self::from_reader(reader, state, settings)?;
					(start.union(part.get_position()), Some(Box::new(part)))
				};
			Ok(Self::PrefixComment(comment, under, position))
		} else {
			let (name, pos) = crate::tokens::token_as_identifier(token, "export name")?;
			let mut value = if let Some(Token(TSXToken::Keyword(TSXKeyword::As), _)) = reader.peek()
			{
				reader.next();
				let token = reader.next().ok_or_else(parse_lexing_error)?;
				let (alias, end) = ImportExportName::from_token(token)?;
				let position = pos.union(end);
				Self::NameWithAlias { name, alias, position }
			} else {
				Self::Name(VariableIdentifier::Standard(name, pos))
			};

			while let Some(Token(TSXToken::MultiLineComment(_), _)) = reader.peek() {
				let Some(Token(TSXToken::MultiLineComment(c), start)) = reader.next() else {
					unreachable!()
				};
				let pos = value.get_position().union(start.get_end_after(c.len() + 2));
				value = Self::PostfixComment(Box::new(value), c, pos)
			}
			Ok(value)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			ExportPart::Name(identifier) => buf.push_str(identifier.as_str()),
			ExportPart::NameWithAlias { name, alias, .. } => {
				buf.push_str(name);
				buf.push_str(" as ");
				match alias {
					ImportExportName::Reference(alias) => buf.push_str(alias),
					ImportExportName::Quoted(alias, q) => {
						buf.push(q.as_char());
						buf.push_str(alias);
						buf.push(q.as_char());
					}
				}
			}
			ExportPart::PrefixComment(comment, inner, _) => {
				if settings.include_comments {
					buf.push_str("/*");
					buf.push_str(&comment);
					buf.push_str("*/");
					if inner.is_some() {
						buf.push(' ');
					}
				}
				if let Some(inner) = inner {
					inner.to_string_from_buffer(buf, settings, depth);
				}
			}
			ExportPart::PostfixComment(inner, comment, _) => {
				inner.to_string_from_buffer(buf, settings, depth);
				if settings.include_comments {
					buf.push_str("/*");
					buf.push_str(&comment);
					buf.push_str("*/ ");
				}
			}
		}
	}
}

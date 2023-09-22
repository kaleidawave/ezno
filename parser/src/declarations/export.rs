use crate::{
	errors::parse_lexing_error, throw_unexpected_token, ASTNode, Expression, Keyword, ParseOptions,
	ParseResult, Span, StatementPosition, TSXKeyword, TSXToken, Token,
};

use super::{
	variable::VariableDeclaration, ClassDeclaration, ImportExportPart, InterfaceDeclaration,
	StatementFunction, TypeAlias,
};

use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::TokenReader;
use visitable_derive::Visitable;

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
	Parts(Vec<ImportExportPart>),
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
		let is_default =
			matches!(reader.peek(), Some(Token(TSXToken::Keyword(TSXKeyword::Default), _)));
		if is_default {
			reader.next();
			let expression = Expression::from_reader(reader, state, settings)?;
			let position = start.union(expression.get_position());
			Ok(ExportDeclaration::Default { expression: Box::new(expression), position })
		} else {
			match reader.peek().ok_or_else(parse_lexing_error)? {
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
					let function_declaration =
						StatementFunction::from_reader(reader, state, settings)?;
					let position = start.union(function_declaration.get_position());
					Ok(Self::Variable {
						exported: Exportable::Function(function_declaration),
						position,
					})
				}
				Token(
					TSXToken::Keyword(TSXKeyword::Const) | TSXToken::Keyword(TSXKeyword::Let),
					_,
				) => {
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
					let (parts, end) = crate::parse_bracketed::<ImportExportPart>(
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
						for (at_end, part) in parts.iter().endiate() {
							part.to_string_from_buffer(buf, settings, depth);
							if !at_end {
								buf.push(',');
							}
						}
						buf.push('}');
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

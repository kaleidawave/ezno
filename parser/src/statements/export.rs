use std::borrow::Cow;

use crate::{
	errors::parse_lexing_error, Expression, Keyword, ParseSettings, StatementPosition, TSXKeyword,
};

use super::{
	ASTNode, ClassDeclaration, InterfaceDeclaration, ParseResult, Span, StatementFunction,
	TSXToken, Token, TokenReader, TypeAlias, VariableStatement,
};

use visitable_derive::Visitable;

/// [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export)
#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum ExportStatement {
	// TODO listed object thing
	// TODO export *
	Variable { exported: Exportable, position: Span },
	// `export default ...`
	Default { expression: Box<Expression>, position: Span },
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum Exportable {
	ClassDeclaration(ClassDeclaration<StatementPosition>),
	FunctionDeclaration(StatementFunction),
	VariableDeclaration(VariableStatement),
	InterfaceDeclaration(InterfaceDeclaration),
	TypeAlias(TypeAlias),
}

// impl Into<Statement> for Exportable {
// 	fn into(self) -> Statement {
// 		match self {
// 			Exportable::ClassDeclaration(cls_dec) => cls_dec.into(),
// 			Exportable::FunctionDeclaration(func_dec) => func_dec.into(),
// 			Exportable::VariableDeclaration(var_dec) => var_dec.into(),
// 			Exportable::InterfaceDeclaration(_) => todo!(),
// 			Exportable::TypeAlias(_) => todo!(),
// 		}
// 	}
// }

impl ASTNode for ExportStatement {
	fn get_position(&self) -> Cow<Span> {
		match self {
			ExportStatement::Variable { position, .. }
			| ExportStatement::Default { position, .. } => Cow::Borrowed(position),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		Self::from_reader_2(reader, state, settings)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		match self {
			ExportStatement::Variable { exported, .. } => match exported {
				Exportable::ClassDeclaration(class_declaration) => {
					class_declaration.to_string_from_buffer(buf, settings, depth);
				}
				Exportable::FunctionDeclaration(function_declaration) => {
					function_declaration.to_string_from_buffer(buf, settings, depth);
				}
				Exportable::InterfaceDeclaration(interface_declaration) => {
					interface_declaration.to_string_from_buffer(buf, settings, depth);
				}
				Exportable::VariableDeclaration(variable_dec_stmt) => {
					buf.push_str("export ");
					variable_dec_stmt.to_string_from_buffer(buf, settings, depth);
				}
				Exportable::TypeAlias(type_alias) => {
					buf.push_str("export ");
					type_alias.to_string_from_buffer(buf, settings, depth);
				}
			},
			ExportStatement::Default { expression, position: _ } => {
				buf.push_str("export default ");
				expression.to_string_from_buffer(buf, settings, depth);
			}
		}
	}
}

impl ExportStatement {
	pub(crate) fn from_reader_2(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Export))?;
		let is_default =
			matches!(reader.peek(), Some(Token(TSXToken::Keyword(TSXKeyword::Default), _)));
		if is_default {
			reader.next();
			let expression = Expression::from_reader(reader, state, settings)?;
			let position = start.union(&expression.get_position());
			Ok(Self::Default { expression: Box::new(expression), position })
		} else {
			match reader.peek().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::Keyword(TSXKeyword::Class), _) => {
					let Token(_, pos) = reader.next().unwrap();
					let keyword = Keyword::new(pos);
					let class_declaration = ClassDeclaration::from_reader_sub_class_keyword(
						reader, state, settings, keyword,
					)?;
					let position = start.union(&class_declaration.get_position());
					Ok(Self::Variable {
						exported: Exportable::ClassDeclaration(class_declaration),
						position,
					})
				}
				Token(TSXToken::Keyword(TSXKeyword::Function), _) => {
					let function_declaration =
						StatementFunction::from_reader(reader, state, settings)?;
					let position = start.union(&function_declaration.get_position());
					Ok(Self::Variable {
						exported: Exportable::FunctionDeclaration(function_declaration),
						position,
					})
				}
				Token(
					TSXToken::Keyword(TSXKeyword::Const) | TSXToken::Keyword(TSXKeyword::Let),
					_,
				) => {
					let variable_declaration =
						VariableStatement::from_reader(reader, state, settings)?;
					let position = start.union(&variable_declaration.get_position());
					Ok(Self::Variable {
						exported: Exportable::VariableDeclaration(variable_declaration),
						position,
					})
				}
				Token(TSXToken::Keyword(TSXKeyword::Interface), _) => {
					let interface_declaration =
						InterfaceDeclaration::from_reader(reader, state, settings)?;
					let position = start.union(&interface_declaration.get_position());
					Ok(Self::Variable {
						exported: Exportable::InterfaceDeclaration(interface_declaration),
						position,
					})
				}
				Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
					let type_alias = TypeAlias::from_reader(reader, state, settings)?;
					let position = start.union(&type_alias.get_position());
					Ok(Self::Variable { exported: Exportable::TypeAlias(type_alias), position })
				}
				Token(token, _) => {
					unimplemented!("Token after export '{:?}'", token)
				}
			}
		}
	}
}

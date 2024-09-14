use crate::{
	derive_ASTNode, type_annotations::TypeAnnotationFunctionParameters,
	types::enum_declaration::EnumDeclaration, ASTNode, Expression, ParseError, ParseOptions,
	ParseResult, Span, StatementPosition, TypeAnnotation, VariableIdentifier,
};

use super::{
	variable::VariableDeclaration, ClassDeclaration, ImportExportPart, ImportLocation,
	InterfaceDeclaration, StatementFunction, TypeAlias,
};

use get_field_by_type::GetFieldByType;
use visitable_derive::Visitable;

/// [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export)
#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ExportDeclaration {
	Item {
		exported: Exportable,
		position: Span,
	},
	// `export default ...`
	Default {
		expression: Box<Expression>,
		position: Span,
	},
	DefaultFunction {
		/// Technically not allowed in TypeScript
		is_async: bool,
		identifier: Option<VariableIdentifier>,
		#[visit_skip_field]
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		position: Span,
	},
}

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable)]
pub enum Exportable {
	Class(ClassDeclaration<StatementPosition>),
	Function(StatementFunction),
	Variable(VariableDeclaration),
	Interface(InterfaceDeclaration),
	TypeAlias(TypeAlias),
	EnumDeclaration(EnumDeclaration),
	Parts(Vec<ImportExportPart<ExportDeclaration>>),
	ImportAll {
		r#as: Option<VariableIdentifier>,
		from: ImportLocation,
	},
	ImportParts {
		// yah `super::ImportDeclaration` here
		parts: Vec<ImportExportPart<super::ImportDeclaration>>,
		from: ImportLocation,
		type_definitions_only: bool,
	},
}

impl ASTNode for ExportDeclaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("export")?;
		reader.skip();

		if reader.is_keyword_advance("default") {
			// reader.peek().map_or(
			// 	false,
			// 	|t| matches!(t.0, TSXToken::Keyword(kw) if kw.is_in_function_header()),
			// ) {
			// let is_async = reader
			// 	.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Async)))
			// 	.is_some();

			// #[allow(unused)]
			// let token = reader.next();
			// debug_assert!(matches!(
			// 	token.unwrap().0,
			// 	TSXToken::Keyword(TSXKeyword::Function)
			// ));

			// let identifier =
			// 	if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek() {
			// 		None
			// 	} else {
			// 		Some(VariableIdentifier::from_reader(reader)?)
			// 	};

			// let parameters =
			// 	TypeAnnotationFunctionParameters::from_reader(reader)?;

			// let return_type = reader
			// 	.conditional_next(|tok| matches!(tok, TSXToken::Colon))
			// 	.is_some()
			// 	.then(|| TypeAnnotation::from_reader(reader))
			// 	.transpose()?;

			// let position = start.union(
			// 	return_type.as_ref().map_or(parameters.position, ASTNode::get_position),
			// );

			// Ok(ExportDeclaration::DefaultFunction {
			// 	position,
			// 	is_async,
			// 	identifier,
			// 	parameters,
			// 	return_type,
			// })
			// }
			let expression = Expression::from_reader(reader)?;
			let position = start.union(expression.get_position());
			Ok(ExportDeclaration::Default { expression: Box::new(expression), position })
		} else if reader.is_operator_advance("*") {
			let r#as = if reader.is_keyword_advance("as") {
				// TODO state.append_keyword_at_pos(reader.next().unwrap().1 .0, TSXKeyword::As);
				Some(VariableIdentifier::from_reader(reader)?)
			} else {
				None
			};

			let start = reader.expect_keyword("from")?;
			// TODO temp
			let from = ImportLocation::from_reader(reader)?;
			let end = reader.get_end();

			Ok(ExportDeclaration::Item {
				exported: Exportable::ImportAll { r#as, from },
				position: start.union(end),
			})
		} else if reader.is_keyword("class") {
			let class_declaration = ClassDeclaration::from_reader(reader)?;
			let position = start.union(class_declaration.get_position());
			Ok(ExportDeclaration::Item { exported: Exportable::Class(class_declaration), position })
		} else if reader.is_one_of_keyword(&["const", "let"]).is_some() {
			let variable_declaration = VariableDeclaration::from_reader(reader)?;
			let position = start.union(variable_declaration.get_position());
			Ok(ExportDeclaration::Item {
				exported: Exportable::Variable(variable_declaration),
				position,
			})
		} else if reader.is_keyword("interface") {
			let interface_declaration = InterfaceDeclaration::from_reader(reader)?;
			let position = start.union(interface_declaration.get_position());
			Ok(ExportDeclaration::Item {
				exported: Exportable::Interface(interface_declaration),
				position,
			})
		} else if reader.is_keyword("type") {
			if reader.get_current()["type".len()..].trim_start().starts_with("{") {
				reader.advance("type".len() as u32);
				let (parts, _) =
					crate::bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;

				let _ = reader.expect_keyword("from")?;

				let from = ImportLocation::from_reader(reader)?;

				let end = reader.get_end();

				Ok(Self::Item {
					exported: Exportable::ImportParts {
						parts,
						from,
						// Important
						type_definitions_only: true,
					},
					position: start.union(end),
				})
			} else {
				let type_alias = TypeAlias::from_reader(reader)?;
				let position = start.union(type_alias.get_position());
				Ok(Self::Item { exported: Exportable::TypeAlias(type_alias), position })
			}
		} else if reader.is_operator_advance("{") {
			// let mut bracket_depth = 1;
			// let after_bracket = reader.scan(|token, _| match token {
			// 	TSXToken::OpenBrace => {
			// 		bracket_depth += 1;
			// 		false
			// 	}
			// 	TSXToken::CloseBrace => {
			// 		bracket_depth -= 1;
			// 		bracket_depth == 0
			// 	}
			// 	_ => false,
			// });
			// if let Some(Token(token_type, _)) = after_bracket {
			// 	if let TSXToken::Keyword(TSXKeyword::From) = token_type {
			// 		let (parts, _, _end) = crate::bracketed_items_from_reader::<ImportExportPart<_>>(
			// 			reader,
			// 			state,
			// 			options,
			// 			None,
			// 			TSXToken::CloseBrace,
			// 		)?;
			// 		let Token(_from_kw, start) = reader.next().unwrap();
			// 		state.append_keyword_at_pos(start.0, TSXKeyword::From);

			// 		let (from, end) =
			// 			ImportLocation::from_reader(reader, Some(start))?;
			// 		Ok(Self::Variable {
			// 			exported: Exportable::ImportParts {
			// 				parts,
			// 				from,
			// 				type_definitions_only: false,
			// 			},
			// 			position: start.union(end),
			// 		})
			// 	} else {
			// 		let (parts, _, end) = crate::bracketed_items_from_reader::<ImportExportPart<_>>(
			// 			reader,
			// 			state,
			// 			options,
			// 			None,
			// 			TSXToken::CloseBrace,
			// 		)?;
			// 		Ok(Self::Variable {
			// 			exported: Exportable::Parts(parts),
			// 			position: start.union(end),
			// 		})
			// 	}
			// } else {
			// 	Err(ParseError::new(
			// 		crate::ParseErrors::UnmatchedBrackets,
			// 		start.with_length(1),
			// 	))
			// }
			todo!()
		} else if reader.is_keyword("var") {
			todo!()
		} else {
			todo!("{:?}", reader.get_current())
			// }
			// Token(TSXToken::Keyword(kw), _) if kw.is_in_function_header() => {
			// 	let function_declaration = StatementFunction::from_reader(reader)?;
			// 	let position = start.union(function_declaration.get_position());
			// 	Ok(Self::Variable {
			// 		exported: Exportable::Function(function_declaration),
			// 		position,
			// 	})
			// }
			// _ => throw_unexpected_token(
			// 	reader,
			// 	&[
			// 		TSXToken::Keyword(TSXKeyword::Class),
			// 		TSXToken::Keyword(TSXKeyword::Function),
			// 		TSXToken::Keyword(TSXKeyword::Const),
			// 		TSXToken::Keyword(TSXKeyword::Let),
			// 		TSXToken::Keyword(TSXKeyword::Interface),
			// 		TSXToken::Keyword(TSXKeyword::Type),
			// 		TSXToken::OpenBrace,
			// 	],
			// ),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			ExportDeclaration::Item { exported, .. } => {
				buf.push_str("export ");
				match exported {
					Exportable::Class(class_declaration) => {
						class_declaration.to_string_from_buffer(buf, options, local);
					}
					Exportable::Function(function_declaration) => {
						function_declaration.to_string_from_buffer(buf, options, local);
					}
					Exportable::Interface(interface_declaration) => {
						interface_declaration.to_string_from_buffer(buf, options, local);
					}
					Exportable::Variable(variable_dec_stmt) => {
						variable_dec_stmt.to_string_from_buffer(buf, options, local);
					}
					Exportable::TypeAlias(type_alias) => {
						type_alias.to_string_from_buffer(buf, options, local);
					}
					Exportable::EnumDeclaration(enum_declaration) => {
						enum_declaration.to_string_from_buffer(buf, options, local);
					}
					Exportable::Parts(parts) => {
						super::import_export_parts_to_string_from_buffer(
							parts, buf, options, local,
						);
					}
					Exportable::ImportAll { r#as, from } => {
						buf.push_str("* ");
						if let Some(r#as) = r#as {
							buf.push_str("as ");
							r#as.to_string_from_buffer(buf, options, local);
							buf.push(' ');
						}
						buf.push_str("from \"");
						from.to_string_from_buffer(buf);
						buf.push('"');
					}
					Exportable::ImportParts { parts, from, type_definitions_only } => {
						if *type_definitions_only {
							buf.push_str("type ");
						}
						super::import_export_parts_to_string_from_buffer(
							parts, buf, options, local,
						);
						options.push_gap_optionally(buf);
						buf.push_str("from \"");
						from.to_string_from_buffer(buf);
						buf.push('"');
					}
				}
			}
			ExportDeclaration::Default { expression, position: _ } => {
				buf.push_str("export default ");
				expression.to_string_from_buffer(buf, options, local);
			}
			ExportDeclaration::DefaultFunction {
				is_async,
				identifier,
				parameters,
				return_type,
				position: _,
			} => {
				if options.include_type_annotations {
					buf.push_str("export default ");
					if *is_async {
						buf.push_str("async ");
					}
					buf.push_str("function ");
					if let Some(ref identifier) = identifier {
						identifier.to_string_from_buffer(buf, options, local);
						buf.push(' ');
					}
					parameters.to_string_from_buffer(buf, options, local);
					if let Some(ref return_type) = return_type {
						buf.push_str(": ");
						return_type.to_string_from_buffer(buf, options, local);
					}
				}
			}
		}
	}
}

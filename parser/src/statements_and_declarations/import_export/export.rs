use crate::{
	derive_ASTNode, type_annotations::TypeAnnotationFunctionParameters,
	types::enum_declaration::EnumDeclaration, ASTNode, Expression, ParseResult, Span,
	StatementPosition, TypeAnnotation, VariableIdentifier,
};

use super::{
	super::{
		variables::VariableDeclaration, ClassDeclaration, InterfaceDeclaration, StatementFunction,
		TypeAlias,
	},
	ImportExportPart, ImportLocation,
};

use get_field_by_type::GetFieldByType;
use visitable_derive::Visitable;

/// [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export)
#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ExportDeclaration {
	/// `export *Exportable*`
	Item { exported: Exportable, position: Span },
	/// `export default ...`
	Default { expression: Box<Expression>, position: Span },
	/// In TypeScript you can `export default name` in type definition modules
	TSDefaultFunctionDeclaration {
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
	VarStatement(crate::statements_and_declarations::variables::VarVariableStatement),
	#[cfg(feature = "full-typescript")]
	Namespace(crate::types::namespace::Namespace),
	Parts(Vec<ImportExportPart<ExportDeclaration>>),
	ImportAll {
		r#as: Option<VariableIdentifier>,
		from: ImportLocation,
	},
	ImportParts {
		parts: Vec<ImportExportPart<super::import::ImportDeclaration>>,
		from: ImportLocation,
		type_definitions_only: bool,
	},
}

impl ASTNode for ExportDeclaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("export")?;
		reader.skip();

		if reader.is_keyword_advance("default") {
			if reader.get_options().type_definition_module
				&& crate::lexer::utilities::is_function_header(reader.get_current())
			{
				// Always have == .d.ts file here
				// Unfortuantly have to do quite a bit of parsing here
				let is_async = reader.is_operator_advance("async");
				let _ = reader.expect_keyword("function");

				let identifier = if reader.is_operator("(") {
					None
				} else {
					Some(VariableIdentifier::from_reader(reader)?)
				};

				let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;

				let return_type = if reader.is_operator_advance(":") {
					Some(TypeAnnotation::from_reader(reader)?)
				} else {
					None
				};

				let position = start.union(reader.get_end());
				Ok(ExportDeclaration::TSDefaultFunctionDeclaration {
					position,
					is_async,
					identifier,
					parameters,
					return_type,
				})
			} else {
				let expression = Expression::from_reader(reader)?;
				let position = start.union(expression.get_position());
				Ok(ExportDeclaration::Default { expression: Box::new(expression), position })
			}
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
		} else if reader.is_operator("{") {
			if reader.after_brackets().starts_with("from") {
				reader.advance(1);

				let (parts, _) =
					crate::bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;
				reader.expect_keyword("from")?;

				let from = ImportLocation::from_reader(reader)?;
				let end = reader.get_end();
				Ok(Self::Item {
					exported: Exportable::ImportParts { parts, from, type_definitions_only: false },
					position: start.union(end),
				})
			} else {
				reader.advance(1);
				let (parts, _) =
					crate::bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;
				let end = reader.get_end();
				Ok(Self::Item { exported: Exportable::Parts(parts), position: start.union(end) })
			}
		} else if reader.is_keyword("class") {
			let class_declaration = ClassDeclaration::from_reader(reader)?;
			let position = start.union(class_declaration.get_position());
			Ok(ExportDeclaration::Item { exported: Exportable::Class(class_declaration), position })
		} else if let Some(keyword) = reader.is_one_of_keywords(&["const", "let"]) {
			if keyword == "const"
				&& reader.get_current()["const".len()..].trim_start().starts_with("enum ")
			{
				let enum_declaration = EnumDeclaration::from_reader(reader)?;
				let position = start.union(enum_declaration.get_position());
				Ok(ExportDeclaration::Item {
					exported: Exportable::EnumDeclaration(enum_declaration),
					position,
				})
			} else {
				let variable_declaration = VariableDeclaration::from_reader(reader)?;
				let position = start.union(variable_declaration.get_position());
				Ok(ExportDeclaration::Item {
					exported: Exportable::Variable(variable_declaration),
					position,
				})
			}
		} else if reader.is_keyword("function") {
			let function_declaration = StatementFunction::from_reader(reader)?;
			let position = start.union(function_declaration.get_position());
			Ok(ExportDeclaration::Item {
				exported: Exportable::Function(function_declaration),
				position,
			})
		} else if reader.is_keyword("var") {
			let var_stmt =
				crate::statements_and_declarations::variables::VarVariableStatement::from_reader(
					reader,
				)?;
			let position = start.union(var_stmt.get_position());
			Ok(ExportDeclaration::Item { exported: Exportable::VarStatement(var_stmt), position })
		} else if reader.is_keyword("interface") {
			let interface_declaration = InterfaceDeclaration::from_reader(reader)?;
			let position = start.union(interface_declaration.get_position());
			Ok(ExportDeclaration::Item {
				exported: Exportable::Interface(interface_declaration),
				position,
			})
		} else if reader.is_keyword("type") {
			if reader.get_current()["type".len()..].trim_start().starts_with('{') {
				reader.advance("type".len() as u32);
				let _ = reader.expect_operator("{");
				let (parts, _) =
					crate::bracketed_items_from_reader::<ImportExportPart<_>>(reader, "}")?;

				let _ = reader.expect_keyword("from")?;
				let from = ImportLocation::from_reader(reader)?;
				let end = reader.get_end();
				let exported = Exportable::ImportParts {
					parts,
					from,
					// Important
					type_definitions_only: true,
				};
				Ok(Self::Item { exported, position: start.union(end) })
			} else {
				let type_alias = TypeAlias::from_reader(reader)?;
				let position = start.union(type_alias.get_position());
				Ok(Self::Item { exported: Exportable::TypeAlias(type_alias), position })
			}
		} else if reader.is_keyword("enum") {
			let enum_declaration = EnumDeclaration::from_reader(reader)?;
			let position = start.union(enum_declaration.get_position());
			// .map(|on| Declaration::Enum(Decorated::new(decorators, on)))
			Ok(ExportDeclaration::Item {
				exported: Exportable::EnumDeclaration(enum_declaration),
				position,
			})
		} else {
			#[cfg(feature = "full-typescript")]
			if reader.is_keyword("namespace") {
				let namespace = crate::types::namespace::Namespace::from_reader(reader)?;
				let position = start.union(namespace.get_position());

				return Ok(Self::Item { exported: Exportable::Namespace(namespace), position });
			}

			// TODO vary list on certain parameters
			Err(crate::lexer::utilities::expected_one_of_items(
				reader,
				&["let", "const", "function", "class", "enum", "type", "interface", "{"],
			))
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
					Exportable::VarStatement(var_stmt) => {
						var_stmt.to_string_from_buffer(buf, options, local);
					}
					#[cfg(feature = "full-typescript")]
					Exportable::Namespace(namespace) => {
						namespace.to_string_from_buffer(buf, options, local);
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
			ExportDeclaration::TSDefaultFunctionDeclaration {
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

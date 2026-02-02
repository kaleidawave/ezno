use crate::{ASTNode, ParseResult, Span, derive_ASTNode};

use crate::expressions::{Expression, ObjectLiteral};
use crate::type_annotations::TypeAnnotationFunctionParameters;
use crate::{TypeAnnotation, VariableIdentifier};

use super::{ImportExportPart, ImportLocation};

use get_field_by_type::GetFieldByType;
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable)]
pub struct Exportable<T> {
	pub is_exported: bool,
	pub item: T,
}

impl<T> Exportable<T> {
	pub fn not_exported(item: T) -> Self {
		Self { is_exported: false, item }
	}

	pub fn exported(item: T) -> Self {
		Self { is_exported: true, item }
	}
}

impl<T: GetFieldByType<Span>> GetFieldByType<Span> for Exportable<T> {
	fn get(&self) -> &Span {
		self.item.get()
	}
}

impl<A: ASTNode> ASTNode for Exportable<A> {
	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let is_exported = reader.is_keyword_advance("export");
		let item = A::from_reader(reader)?;
		Ok(Self { is_exported, item })
	}

	// TODO
	fn get_position(&self) -> Span {
		self.item.get_position()
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if self.is_exported {
			buf.push_str("export ");
		}
		self.item.to_string_from_buffer(buf, options, local);
	}
}

/// [See](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export)
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum ExportDeclaration {
	/// `export { ... }`
	Parts(Vec<ImportExportPart<ExportDeclaration>>, Span),
	/// `export * as x from "..."`
	ImportToExportAll {
		r#as: Option<VariableIdentifier>,
		from: ImportLocation,
		with: Option<ObjectLiteral>,
		position: Span,
	},
	/// `export { ... } from "..."`
	ImportToExportParts {
		parts: Vec<ImportExportPart<super::import::ImportDeclaration>>,
		from: ImportLocation,
		with: Option<ObjectLiteral>,
		type_definitions_only: bool,
		position: Span,
	},
	/// `export default ...`
	Default { expression: Box<Expression>, position: Span },
	/// In TypeScript you can `export default function (): string` in type definition modules
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

pub(crate) fn export_declaration_from_reader_after_export_keyword(
	start: source_map::Start,
	reader: &mut crate::Lexer,
) -> ParseResult<ExportDeclaration> {
	reader.skip();
	if reader.is_keyword_advance("default") {
		let edge_case = reader.get_options().type_annotations.is_definition_file()
			&& reader.starts_with_function_header();
		// Always have == .d.ts file here
		// Unfortuantly have to do quite a bit of parsing here
		if edge_case {
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
			// TODO check expression here
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
		let position = start.union(end);

		let with = reader
			.is_operator_advance("with")
			.then(|| ObjectLiteral::from_reader(reader))
			.transpose()?;

		Ok(ExportDeclaration::ImportToExportAll { r#as, from, position, with })
	} else if reader.is_operator_advance("{") {
		let type_definitions_only = false;
		let (parts, _) =
			crate::bracketed_items_from_reader::<ImportExportPart<ExportDeclaration>>(reader, "}")?;

		if reader.is_keyword_advance("from") {
			let from = ImportLocation::from_reader(reader)?;

			let parts: Vec<_> = parts.into_iter().map(ImportExportPart::into_import_form).collect();

			let with = reader
				.is_operator_advance("with")
				.then(|| ObjectLiteral::from_reader(reader))
				.transpose()?;
			let position = start.union(reader.get_end());

			Ok(ExportDeclaration::ImportToExportParts {
				parts,
				from,
				type_definitions_only,
				with,
				position,
			})
		} else {
			let position = start.union(reader.get_end());
			Ok(ExportDeclaration::Parts(parts, position))
		}
	} else if reader.parse_type_annotations() && reader.is_keyword_advance("type") {
		reader.expect_operator("{")?;
		let type_definitions_only = true;
		let (parts, _) =
			crate::bracketed_items_from_reader::<ImportExportPart<ExportDeclaration>>(reader, "}")?;

		if reader.is_keyword_advance("from") {
			let from = ImportLocation::from_reader(reader)?;

			let parts: Vec<_> = parts.into_iter().map(ImportExportPart::into_import_form).collect();

			let with = reader
				.is_operator_advance("with")
				.then(|| ObjectLiteral::from_reader(reader))
				.transpose()?;
			let position = start.union(reader.get_end());

			Ok(ExportDeclaration::ImportToExportParts {
				parts,
				from,
				type_definitions_only,
				with,
				position,
			})
		} else {
			let position = start.union(reader.get_end());
			Ok(ExportDeclaration::Parts(parts, position))
		}
	} else {
		Err(crate::lexer::utilities::expected_one_of_items(reader, &["{", "*", "default", "type"]))
	}
}

impl ASTNode for ExportDeclaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("export")?;
		export_declaration_from_reader_after_export_keyword(start, reader)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("export ");
		match self {
			ExportDeclaration::Parts(parts, _) => {
				super::import_export_parts_to_string_from_buffer(parts, buf, options, local);
			}
			ExportDeclaration::ImportToExportAll { r#as, from, with, position: _ } => {
				buf.push_str("* ");
				if let Some(r#as) = r#as {
					buf.push_str("as ");
					r#as.to_string_from_buffer(buf, options, local);
					buf.push(' ');
				}
				buf.push_str("from");
				options.push_gap_optionally(buf);
				buf.push('"');
				from.to_string_from_buffer(buf);
				buf.push('"');
				if let Some(with) = with {
					buf.push_str("with ");
					with.to_string_from_buffer(buf, options, local);
				}
			}
			ExportDeclaration::ImportToExportParts {
				parts,
				from,
				type_definitions_only,
				with,
				position: _,
			} => {
				if *type_definitions_only {
					buf.push_str("type ");
				}
				super::import_export_parts_to_string_from_buffer(parts, buf, options, local);
				options.push_gap_optionally(buf);
				buf.push_str("from");
				options.push_gap_optionally(buf);
				buf.push('"');
				from.to_string_from_buffer(buf);
				buf.push('"');

				if let Some(with) = with {
					buf.push_str("with ");
					with.to_string_from_buffer(buf, options, local);
				}
			}
			ExportDeclaration::Default { expression, position: _ } => {
				buf.push_str("default ");
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
					buf.push_str("default ");
					if *is_async {
						buf.push_str("async ");
					}
					buf.push_str("function ");
					if let Some(identifier) = identifier {
						identifier.to_string_from_buffer(buf, options, local);
						buf.push(' ');
					}
					parameters.to_string_from_buffer(buf, options, local);
					if let Some(return_type) = return_type {
						buf.push_str(": ");
						return_type.to_string_from_buffer(buf, options, local);
					}
				}
			}
		}
	}
}

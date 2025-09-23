use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use visitable_derive::Visitable;

/// re-export
pub use crate::VariableField;
use crate::{
	derive_ASTNode, ASTNode, Expression, ParseError, ParseResult, Span, TypeAnnotation, WithComment,
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum VariableKeyword {
	Const,
	Let,
	Var,
}

impl VariableKeyword {
	pub(crate) fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.is_keyword_advance("const") {
			Ok(Self::Const)
		} else if reader.is_keyword_advance("let") {
			Ok(Self::Let)
		} else if reader.is_keyword_advance("var") {
			Ok(Self::Var)
		} else {
			let error =
				crate::lexer::utilities::expected_one_of_items(reader, &["const", "let", "var"]);
			Err(error)
		}
	}

	#[must_use]
	pub fn as_str(&self) -> &str {
		match self {
			Self::Const => "const ",
			Self::Let => "let ",
			Self::Var => "var ",
		}
	}
}

/// Represents a name =
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct VariableDeclarationItem {
	pub name: WithComment<VariableField>,
	pub type_annotation: Option<TypeAnnotation>,
	/// `const` declarations require this to be some but it is an error during parsing
	pub expression: Option<Expression>,
	pub position: Span,
}

impl ASTNode for VariableDeclarationItem {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let name = WithComment::<VariableField>::from_reader(reader)?;
		let mut position = name.get_position();
		// if TExpr::allow_definite_assignment_assertions() {
		// TODO
		if true {
			let _ = reader.is_operator_advance("!");
		}

		let type_annotation = if reader.is_operator_advance(":") {
			let annotation = TypeAnnotation::from_reader(reader)?;
			position = position.union(annotation.get_position());
			crate::lexer::utilities::assert_type_annotations(reader, annotation.get_position())?;
			Some(annotation)
		} else {
			None
		};
		let expression = if reader.is_operator_advance("=") {
			let expression = Expression::from_reader(reader)?;
			position = position.union(expression.get_position());
			Some(expression)
		} else {
			None
		};

		Ok(Self { name, type_annotation, expression, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.name.to_string_from_buffer(buf, options, local);
		if let (true, Some(type_annotation)) =
			(options.include_type_annotations, &self.type_annotation)
		{
			buf.push_str(": ");
			type_annotation.to_string_from_buffer(buf, options, local);
		}
		if let Some(ref expression) = self.expression {
			buf.push_str(if options.pretty { " = " } else { "=" });
			expression.to_string_from_buffer(buf, options, local);
		}
	}
}

/// Not `var` because that is a statement 🤦‍♂️
#[derive(Debug, Clone, Copy, Visitable)]
#[apply(derive_ASTNode)]
pub enum VariableDeclarationKeyword {
	Const,
	Let,
}

impl VariableDeclarationKeyword {
	#[must_use]
	pub fn as_str(&self) -> &str {
		match self {
			VariableDeclarationKeyword::Const => "const ",
			VariableDeclarationKeyword::Let => "let ",
		}
	}

	pub fn from_reader(reader: &mut crate::Lexer) -> Option<Self> {
		if reader.is_keyword_advance("const") {
			Some(VariableDeclarationKeyword::Const)
		} else if reader.is_keyword_advance("let") {
			Some(VariableDeclarationKeyword::Let)
		} else {
			None
		}
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct VariableDeclaration {
	pub kind: VariableDeclarationKeyword,
	pub declarations: Vec<VariableDeclarationItem>,
	pub position: Span,
}

impl ASTNode for VariableDeclaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.skip();
		let start = reader.get_start();
		if let Some(kind) = VariableDeclarationKeyword::from_reader(reader) {
			// state.append_keyword_at_pos(start.0, TSXKeyword::Let);
			let mut declarations = Vec::new();
			loop {
				reader.skip();
				if reader.is_one_of(&["//", "/*"]).is_some() {
					let is_multiline = reader.starts_with_slice("/*");
					reader.advance(2);
					let _content = reader.parse_comment_literal(is_multiline)?;
					continue;
				}

				let value = VariableDeclarationItem::from_reader(reader)?;

				if value.expression.is_none() {
					if let VariableDeclarationKeyword::Const = kind {
						return Err(crate::ParseError::new(
							crate::ParseErrors::ConstDeclarationRequiresValue,
							value.name.get_ast_ref().get_position(),
						));
					}
					if !matches!(value.name.get_ast_ref(), VariableField::Name(_)) {
						return Err(crate::ParseError::new(
							crate::ParseErrors::DestructuringRequiresValue,
							value.name.get_ast_ref().get_position(),
						));
					}
				}

				declarations.push(value);
				if !reader.is_operator_advance(",") {
					break;
				}
			}

			let position = start.union(reader.get_end());

			Ok(VariableDeclaration { kind, declarations, position })
		} else {
			Err(crate::lexer::utilities::expected_one_of_items(reader, &["const", "let"]))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if self.declarations.is_empty() {
			return;
		}
		buf.push_str(self.kind.as_str());
		let available_space =
			u32::from(options.max_line_length).saturating_sub(buf.characters_on_current_line());

		let split_lines = crate::are_nodes_over_length(
			self.declarations.iter(),
			options,
			local,
			Some(available_space),
			true,
		);
		declarations_to_string(&self.declarations, buf, options, local, split_lines);
	}
}

impl VariableDeclaration {
	#[must_use]
	pub fn is_constant(&self) -> bool {
		matches!(self.kind, VariableDeclarationKeyword::Const)
	}
}

pub(crate) fn declarations_to_string<T: source_map::ToString>(
	declarations: &[VariableDeclarationItem],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
	separate_lines: bool,
) {
	for (at_end, declaration) in declarations.iter().endiate() {
		declaration.to_string_from_buffer(buf, options, local);
		if !at_end {
			buf.push(',');
			if separate_lines {
				buf.push_new_line();
				options.add_indent(local.depth + 1, buf);
			} else {
				options.push_gap_optionally(buf);
			}
		}
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct VarVariableStatement {
	pub declarations: Vec<VariableDeclarationItem>,
	pub position: Span,
}

impl ASTNode for VarVariableStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let _ = reader.expect_keyword("var")?;
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::from_reader(reader)?;
			if value.expression.is_none()
				&& !matches!(value.name.get_ast_ref(), crate::VariableField::Name(_))
			{
				return Err(crate::ParseError::new(
					crate::ParseErrors::DestructuringRequiresValue,
					value.name.get_ast_ref().get_position(),
				));
			}
			declarations.push(value);
			if !reader.is_operator_advance(",") {
				break;
			}
		}

		let position = if let Some(last) = declarations.last() {
			start.union(last.get_position())
		} else {
			let position = start.with_length(3);
			if reader.get_options().partial_syntax {
				position
			} else {
				return Err(ParseError::new(crate::ParseErrors::ExpectedDeclaration, position));
			}
		};

		Ok(VarVariableStatement { declarations, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("var ");
		declarations_to_string(&self.declarations, buf, options, local, false);
	}
}

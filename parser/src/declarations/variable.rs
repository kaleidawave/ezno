use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	derive_ASTNode, ASTNode, Expression, ParseError, ParseErrors, ParseResult, Span,
	TypeAnnotation, VariableField, WithComment,
};
use visitable_derive::Visitable;

/// This is for `const` declarations vs `let` and `var` declarations
pub trait DeclarationExpression:
	PartialEq + Clone + std::fmt::Debug + Send + std::marker::Sync + crate::Visitable
{
	fn expression_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self>;

	fn expression_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	);

	fn get_declaration_position(&self) -> Option<Span>;

	fn as_option_expression_ref(&self) -> Option<&Expression>;

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression>;

	/// TS nonsence
	fn allow_definite_assignment_assertions() -> bool;
}

impl DeclarationExpression for Option<Expression> {
	fn expression_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.is_operator_advance("=") {
			Expression::from_reader(reader).map(Some)
		} else {
			Ok(None)
		}
	}

	fn expression_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(expr) = self {
			buf.push_str(if options.pretty { " = " } else { "=" });
			expr.to_string_from_buffer(buf, options, local);
		}
	}

	fn get_declaration_position(&self) -> Option<Span> {
		self.as_ref().map(ASTNode::get_position)
	}

	fn as_option_expression_ref(&self) -> Option<&Expression> {
		self.as_ref()
	}

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression> {
		self.as_mut()
	}

	fn allow_definite_assignment_assertions() -> bool {
		true
	}
}

impl DeclarationExpression for crate::Expression {
	fn expression_from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let _start = reader.expect('=')?;
		Expression::from_reader(reader)
	}

	fn expression_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str(if options.pretty { " = " } else { "=" });
		ASTNode::to_string_from_buffer(self, buf, options, local);
	}

	fn get_declaration_position(&self) -> Option<Span> {
		Some(ASTNode::get_position(self))
	}

	fn as_option_expression_ref(&self) -> Option<&Expression> {
		Some(self)
	}

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression> {
		Some(self)
	}

	fn allow_definite_assignment_assertions() -> bool {
		false
	}
}

/// Represents a name =
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
pub struct VariableDeclarationItem<TExpr: DeclarationExpression> {
	pub name: WithComment<VariableField>,
	pub type_annotation: Option<TypeAnnotation>,
	pub expression: TExpr,
	pub position: Span,
}

impl<TExpr: DeclarationExpression + 'static> ASTNode for VariableDeclarationItem<TExpr> {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let name = WithComment::<VariableField>::from_reader(reader)?;
		if TExpr::allow_definite_assignment_assertions() {
			let _ = reader.is_operator_advance("!");
		}

		let type_annotation = if reader.is_operator_advance(":") {
			let annotation = TypeAnnotation::from_reader(reader)?;
			crate::lexer::utilities::assert_type_annotations(reader, annotation.get_position())?;
			Some(annotation)
		} else {
			None
		};
		let expression = TExpr::expression_from_reader(reader)?;
		let position = name.get_position().union(
			expression
				.get_declaration_position()
				.or(type_annotation.as_ref().map(ASTNode::get_position))
				.unwrap_or(name.get_position()),
		);

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

		self.expression.expression_to_string_from_buffer(buf, options, local);
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable, get_field_by_type::GetFieldByType)]
#[partial_eq_ignore_types(Span)]
#[get_field_by_type_target(Span)]
pub enum VariableDeclaration {
	ConstDeclaration {
		declarations: Vec<VariableDeclarationItem<Expression>>,
		position: Span,
	},
	LetDeclaration {
		declarations: Vec<VariableDeclarationItem<Option<Expression>>>,
		position: Span,
	},
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Visitable)]
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
}

impl ASTNode for VariableDeclaration {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		if reader.is_keyword_advance("let") {
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

				let value = VariableDeclarationItem::<Option<Expression>>::from_reader(reader)?;

				if value.expression.is_none()
					&& !matches!(value.name.get_ast_ref(), VariableField::Name(_))
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
				// if options.partial_syntax {
				// 	position
				// } else {
				// }
				return Err(ParseError::new(ParseErrors::ExpectedDeclaration, position));
			};

			Ok(VariableDeclaration::LetDeclaration { position, declarations })
		} else if reader.is_keyword_advance("const") {
			// state.append_keyword_at_pos(start.0, TSXKeyword::Const);
			let mut declarations = Vec::new();
			loop {
				// Some people like to have trailing comments in declarations ?
				// if reader.peek().is_some_and(|t| t.0.is_comment()) {
				// 	let (..) = TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
				// 	if reader.peek_n(1).is_some_and(|t| !t.0.is_identifier_or_ident()) {
				// 		break;
				// 	}
				// 	continue;
				// }
				declarations.push(VariableDeclarationItem::<Expression>::from_reader(reader)?);
				if !reader.is_operator_advance(",") {
					break;
				}
			}

			let position = if let Some(last) = declarations.last() {
				start.union(last.get_position())
			} else {
				let position = start.with_length(3);
				// if options.partial_syntax {
				// 	position
				// } else {
				// }
				return Err(ParseError::new(ParseErrors::ExpectedDeclaration, position));
			};

			Ok(VariableDeclaration::ConstDeclaration { position, declarations })
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
		match self {
			VariableDeclaration::LetDeclaration { declarations, .. } => {
				if declarations.is_empty() {
					return;
				}
				buf.push_str("let ");
				let available_space = u32::from(options.max_line_length)
					.saturating_sub(buf.characters_on_current_line());

				let split_lines = crate::are_nodes_over_length(
					declarations.iter(),
					options,
					local,
					Some(available_space),
					true,
				);
				declarations_to_string(declarations, buf, options, local, split_lines);
			}
			VariableDeclaration::ConstDeclaration { declarations, .. } => {
				if declarations.is_empty() {
					return;
				}
				buf.push_str("const ");
				let available_space = u32::from(options.max_line_length)
					.saturating_sub(buf.characters_on_current_line());

				let split_lines = crate::are_nodes_over_length(
					declarations.iter(),
					options,
					local,
					Some(available_space),
					true,
				);
				declarations_to_string(declarations, buf, options, local, split_lines);
			}
		}
	}
}

impl VariableDeclaration {
	#[must_use]
	pub fn is_constant(&self) -> bool {
		matches!(self, VariableDeclaration::ConstDeclaration { .. })
	}
}

pub(crate) fn declarations_to_string<
	T: source_map::ToString,
	U: DeclarationExpression + 'static,
>(
	declarations: &[VariableDeclarationItem<U>],
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

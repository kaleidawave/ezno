use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	derive_ASTNode, errors::parse_lexing_error, expressions::operators::COMMA_PRECEDENCE,
	throw_unexpected_token_with_token, ASTNode, Expression, ParseOptions, ParseResult, Span,
	TSXKeyword, TSXToken, Token, TokenReader, TypeAnnotation, VariableField, WithComment,
};
use visitable_derive::Visitable;

/// This is for `const` declarations vs `let` and `var` declarations
pub trait DeclarationExpression:
	PartialEq + Clone + std::fmt::Debug + Send + std::marker::Sync + crate::Visitable
{
	fn expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self>;

	fn expression_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	);

	fn get_declaration_position(&self) -> Option<Span>;

	fn as_option_expression_ref(&self) -> Option<&Expression>;

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression>;
}

impl DeclarationExpression for Option<Expression> {
	fn expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		// expect_value: bool,
	) -> ParseResult<Self> {
		if let Some(Token(_, start)) = reader.conditional_next(|t| matches!(t, TSXToken::Assign)) {
			Expression::from_reader_with_precedence(
				reader,
				state,
				options,
				COMMA_PRECEDENCE,
				Some(start),
			)
			.map(Some)
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
}

impl DeclarationExpression for crate::Expression {
	fn expression_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Assign)?;
		Expression::from_reader_with_precedence(
			reader,
			state,
			options,
			COMMA_PRECEDENCE,
			Some(start),
		)
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
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let name = WithComment::<VariableField>::from_reader(reader, state, options)?;
		let type_annotation = if reader
			.conditional_next(|tok| options.type_annotations && matches!(tok, TSXToken::Colon))
			.is_some()
		{
			let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
			Some(type_annotation)
		} else {
			None
		};
		let expression = TExpr::expression_from_reader(reader, state, options)?;
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

	fn get_position(&self) -> Span {
		*self.get()
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
	pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
		matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let))
	}

	pub(crate) fn from_token(token: Token<TSXToken, crate::TokenStart>) -> ParseResult<Self> {
		match token {
			Token(TSXToken::Keyword(TSXKeyword::Const), _) => Ok(Self::Const),
			Token(TSXToken::Keyword(TSXKeyword::Let), _) => Ok(Self::Let),
			token => throw_unexpected_token_with_token(
				token,
				&[TSXToken::Keyword(TSXKeyword::Const), TSXToken::Keyword(TSXKeyword::Let)],
			),
		}
	}

	#[must_use]
	pub fn as_str(&self) -> &str {
		match self {
			VariableDeclarationKeyword::Const => "const ",
			VariableDeclarationKeyword::Let => "let ",
		}
	}
}

impl ASTNode for VariableDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let start = token.1;
		let kind = VariableDeclarationKeyword::from_token(token)?;
		Ok(match kind {
			VariableDeclarationKeyword::Let => {
				state.append_keyword_at_pos(start.0, TSXKeyword::Let);
				let mut declarations = Vec::new();
				loop {
					// Some people like to have trailing comments in declarations ?
					if reader.peek().is_some_and(|t| t.0.is_comment()) {
						let (..) = TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
						if reader.peek_n(1).is_some_and(|t| !t.0.is_identifier_or_ident()) {
							break;
						}
						continue;
					}

					let value = VariableDeclarationItem::<Option<Expression>>::from_reader(
						reader, state, options,
					)?;

					if value.expression.is_none()
						&& !matches!(value.name.get_ast_ref(), VariableField::Name(_))
					{
						return Err(crate::ParseError::new(
							crate::ParseErrors::DestructuringRequiresValue,
							value.name.get_ast_ref().get_position(),
						));
					}

					declarations.push(value);
					if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
						reader.next();
					} else {
						break;
					}
				}
				VariableDeclaration::LetDeclaration {
					position: start.union(declarations.last().unwrap().get_position()),
					declarations,
				}
			}
			VariableDeclarationKeyword::Const => {
				state.append_keyword_at_pos(start.0, TSXKeyword::Const);
				let mut declarations = Vec::new();
				loop {
					// Some people like to have trailing comments in declarations ?
					if reader.peek().is_some_and(|t| t.0.is_comment()) {
						let (..) = TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
						if reader.peek_n(1).is_some_and(|t| !t.0.is_identifier_or_ident()) {
							break;
						}
						continue;
					}

					let value =
						VariableDeclarationItem::<Expression>::from_reader(reader, state, options)?;
					declarations.push(value);
					if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
						reader.next();
					} else {
						break;
					}
				}
				VariableDeclaration::ConstDeclaration {
					position: start.union(declarations.last().unwrap().get_position()),
					declarations,
				}
			}
		})
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

	fn get_position(&self) -> Span {
		*self.get()
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

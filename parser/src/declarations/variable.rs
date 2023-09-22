use derive_partial_eq_extras::PartialEqExtras;
use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	errors::parse_lexing_error, throw_unexpected_token_with_token, tsx_keywords, ASTNode,
	Expression, Keyword, ParseOptions, ParseResult, Span, TSXKeyword, TSXToken, Token, TokenReader,
	TypeAnnotation, VariableField, VariableFieldInSourceCode, WithComment,
};
use visitable_derive::Visitable;

/// This is for `const` declarations vs `let` and `var` declarations
pub trait DeclarationExpression:
	PartialEq + Clone + std::fmt::Debug + Send + std::marker::Sync + crate::Visitable
{
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self>;

	fn decl_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	);

	fn get_decl_position(&self) -> Option<&Span>;

	fn as_option_expr_ref(&self) -> Option<&Expression>;

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression>;
}

impl DeclarationExpression for Option<Expression> {
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::Assign, _)) = reader.peek() {
			reader.next();
			let expression = Expression::from_reader(reader, state, settings)?;
			Ok(Some(expression))
		} else {
			Ok(None)
		}
	}

	fn decl_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if let Some(expr) = self {
			buf.push_str(if settings.pretty { " = " } else { "=" });
			expr.to_string_from_buffer(buf, settings, depth)
		}
	}

	fn get_decl_position(&self) -> Option<&Span> {
		self.as_ref().map(|expr| expr.get_position())
	}

	fn as_option_expr_ref(&self) -> Option<&Expression> {
		self.as_ref()
	}

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression> {
		self.as_mut()
	}
}

impl DeclarationExpression for crate::Expression {
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Assign)?;
		Expression::from_reader(reader, state, settings)
	}

	fn decl_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str(if settings.pretty { " = " } else { "=" });
		ASTNode::to_string_from_buffer(self, buf, settings, depth)
	}

	fn get_decl_position(&self) -> Option<&Span> {
		Some(ASTNode::get_position(self))
	}

	fn as_option_expr_ref(&self) -> Option<&Expression> {
		Some(self)
	}

	fn as_option_expr_mut(&mut self) -> Option<&mut Expression> {
		Some(self)
	}
}

/// Represents a name =
#[derive(Debug, Clone, PartialEqExtras, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct VariableDeclarationItem<TExpr: DeclarationExpression> {
	pub name: WithComment<VariableField<VariableFieldInSourceCode>>,
	pub type_annotation: Option<TypeAnnotation>,
	pub expression: TExpr,
	pub position: Span,
}

impl<TExpr: DeclarationExpression + 'static> ASTNode for VariableDeclarationItem<TExpr> {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let name = WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
			reader, state, settings,
		)?;
		let type_annotation = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
			reader.next();
			let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
			Some(type_annotation)
		} else {
			None
		};
		let expression = TExpr::decl_from_reader(reader, state, settings)?;
		Ok(Self {
			position: name.get_position().union(
				expression
					.get_decl_position()
					.or(type_annotation.as_ref().map(|ta| ta.get_position()))
					// TODO lol
					.unwrap_or(name.get_position()),
			),
			name,
			type_annotation,
			expression,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		self.name.to_string_from_buffer(buf, settings, depth);
		if let (true, Some(type_annotation)) = (settings.include_types, &self.type_annotation) {
			buf.push_str(": ");
			type_annotation.to_string_from_buffer(buf, settings, depth);
		}
		self.expression.decl_to_string_from_buffer(buf, settings, depth);
	}

	fn get_position(&self) -> &Span {
		self.get()
	}
}

/// TODO smallvec the declarations
#[derive(Debug, Clone, PartialEqExtras, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[partial_eq_ignore_types(Span)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum VariableDeclaration {
	ConstDeclaration {
		keyword: Keyword<tsx_keywords::Const>,
		declarations: Vec<VariableDeclarationItem<Expression>>,
		position: Span,
	},
	LetDeclaration {
		keyword: Keyword<tsx_keywords::Let>,
		declarations: Vec<VariableDeclarationItem<Option<Expression>>>,
		position: Span,
	},
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum VariableDeclarationKeyword {
	Const(Keyword<tsx_keywords::Const>),
	Let(Keyword<tsx_keywords::Let>),
}

impl VariableDeclarationKeyword {
	pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
		matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let))
	}

	pub(crate) fn from_reader(token: Token<TSXToken, crate::TokenStart>) -> ParseResult<Self> {
		let position = token.get_span();
		match token {
			Token(TSXToken::Keyword(TSXKeyword::Const), _) => {
				Ok(Self::Const(Keyword::new(position)))
			}
			Token(TSXToken::Keyword(TSXKeyword::Let), _) => Ok(Self::Let(Keyword::new(position))),
			token => throw_unexpected_token_with_token(
				token,
				&[TSXToken::Keyword(TSXKeyword::Const), TSXToken::Keyword(TSXKeyword::Let)],
			),
		}
	}

	pub fn as_str(&self) -> &str {
		match self {
			VariableDeclarationKeyword::Const(_) => "const ",
			VariableDeclarationKeyword::Let(_) => "let ",
		}
	}

	pub fn get_position(&self) -> &Span {
		match self {
			VariableDeclarationKeyword::Const(kw) => kw.get_position(),
			VariableDeclarationKeyword::Let(kw) => kw.get_position(),
		}
	}
}

impl ASTNode for VariableDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let kind = VariableDeclarationKeyword::from_reader(token)?;
		Ok(match kind {
			VariableDeclarationKeyword::Let(keyword) => {
				let mut declarations = Vec::new();
				loop {
					let value = VariableDeclarationItem::<Option<Expression>>::from_reader(
						reader, state, settings,
					)?;
					declarations.push(value);
					if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
						reader.next();
					} else {
						break;
					}
				}
				VariableDeclaration::LetDeclaration {
					position: keyword
						.get_position()
						.union(declarations.last().unwrap().get_position()),
					keyword,
					declarations,
				}
			}
			VariableDeclarationKeyword::Const(keyword) => {
				let mut declarations = Vec::new();
				loop {
					let value = VariableDeclarationItem::<Expression>::from_reader(
						reader, state, settings,
					)?;
					declarations.push(value);
					if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
						reader.next();
					} else {
						break;
					}
				}
				VariableDeclaration::ConstDeclaration {
					position: keyword
						.get_position()
						.union(declarations.last().unwrap().get_position()),
					keyword,
					declarations,
				}
			}
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			VariableDeclaration::LetDeclaration { declarations, .. } => {
				buf.push_str("let ");
				declarations_to_string(declarations, buf, settings, depth);
			}
			VariableDeclaration::ConstDeclaration { declarations, .. } => {
				buf.push_str("const ");
				declarations_to_string(declarations, buf, settings, depth);
			}
		}
	}

	fn get_position(&self) -> &Span {
		self.get()
	}
}

impl VariableDeclaration {
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
	settings: &crate::ToStringOptions,
	depth: u8,
) {
	for (at_end, declaration) in declarations.iter().endiate() {
		declaration.to_string_from_buffer(buf, settings, depth);
		if !at_end {
			buf.push(',');
			settings.add_gap(buf);
		}
	}
}

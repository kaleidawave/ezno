use std::borrow::Cow;

use iterator_endiate::EndiateIteratorExt;

use crate::{
	errors::parse_lexing_error, tsx_keywords, ASTNode, Expression, Keyword, ParseError,
	ParseResult, ParseSettings, Span, TSXKeyword, TSXToken, Token, TokenReader, TypeReference,
	VariableField, VariableFieldInSourceCode, WithComment,
};
use visitable_derive::Visitable;

/// This is for `const` declarations vs `let` and `var` declarations
pub trait DeclarationExpression:
	PartialEq + Clone + std::fmt::Debug + Send + std::marker::Sync + crate::Visitable
{
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self>;

	fn decl_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	);

	fn get_decl_position(&self) -> Option<Cow<Span>>;

	fn as_option_mut_expr(&mut self) -> Option<&mut Expression>;
}

impl DeclarationExpression for Option<Expression> {
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
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
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		if let Some(expr) = self {
			buf.push_str(if settings.pretty { " = " } else { "=" });
			expr.to_string_from_buffer(buf, settings, depth)
		}
	}

	fn get_decl_position(&self) -> Option<Cow<Span>> {
		self.as_ref().map(|expr| expr.get_position())
	}

	fn as_option_mut_expr(&mut self) -> Option<&mut Expression> {
		self.as_mut()
	}
}

impl DeclarationExpression for crate::Expression {
	fn decl_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Assign)?;
		Expression::from_reader(reader, state, settings)
	}

	fn decl_to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push_str(if settings.pretty { " = " } else { "=" });
		ASTNode::to_string_from_buffer(self, buf, settings, depth)
	}

	fn get_decl_position(&self) -> Option<Cow<Span>> {
		Some(ASTNode::get_position(self))
	}

	fn as_option_mut_expr(&mut self) -> Option<&mut Expression> {
		Some(self)
	}
}

/// Represents a name =
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct VariableDeclarationItem<TExpr: DeclarationExpression> {
	pub name: WithComment<VariableField<VariableFieldInSourceCode>>,
	pub type_reference: Option<TypeReference>,
	pub expression: TExpr,
}

impl<TExpr: DeclarationExpression + 'static> ASTNode for VariableDeclarationItem<TExpr> {
	fn get_position(&self) -> Cow<Span> {
		let name_position = self.name.get_position();
		if let Some(expr_pos) = TExpr::get_decl_position(&self.expression) {
			Cow::Owned(name_position.union(&expr_pos))
		} else if let Some(ref ty_ref) = self.type_reference {
			Cow::Owned(name_position.union(&ty_ref.get_position()))
		} else {
			name_position
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let name = WithComment::<VariableField<VariableFieldInSourceCode>>::from_reader(
			reader, state, settings,
		)?;
		let type_reference = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
			reader.next();
			let type_reference = TypeReference::from_reader(reader, state, settings)?;
			Some(type_reference)
		} else {
			None
		};
		let expression = TExpr::decl_from_reader(reader, state, settings)?;
		Ok(Self { name, type_reference, expression })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		self.name.to_string_from_buffer(buf, settings, depth);
		if let (true, Some(type_reference)) = (settings.include_types, &self.type_reference) {
			buf.push_str(": ");
			type_reference.to_string_from_buffer(buf, settings, depth);
		}
		self.expression.decl_to_string_from_buffer(buf, settings, depth);
	}
}

/// TODO smallvec the declarations
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum VariableDeclaration {
	ConstDeclaration {
		keyword: Keyword<tsx_keywords::Const>,
		declarations: Vec<VariableDeclarationItem<Expression>>,
	},
	LetDeclaration {
		keyword: Keyword<tsx_keywords::Let>,
		declarations: Vec<VariableDeclarationItem<Option<Expression>>>,
	},
}

#[derive(Debug, PartialEq, Eq, Clone, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum VariableDeclarationKeyword {
	Const(Keyword<tsx_keywords::Const>),
	Let(Keyword<tsx_keywords::Let>),
}

impl VariableDeclarationKeyword {
	pub fn is_token_variable_keyword(token: &TSXToken) -> bool {
		matches!(token, TSXToken::Keyword(TSXKeyword::Const | TSXKeyword::Let))
	}

	pub(crate) fn from_reader(token: Token<TSXToken, Span>) -> ParseResult<Self> {
		match token {
			Token(TSXToken::Keyword(TSXKeyword::Const), pos) => Ok(Self::Const(Keyword::new(pos))),
			Token(TSXToken::Keyword(TSXKeyword::Let), pos) => Ok(Self::Let(Keyword::new(pos))),
			Token(token, position) => Err(ParseError::new(
				crate::ParseErrors::UnexpectedToken {
					expected: &[
						TSXToken::Keyword(TSXKeyword::Const),
						TSXToken::Keyword(TSXKeyword::Let),
					],
					found: token,
				},
				position,
			)),
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
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let kind =
			VariableDeclarationKeyword::from_reader(reader.next().ok_or_else(parse_lexing_error)?)?;
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
				VariableDeclaration::LetDeclaration { keyword, declarations }
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
				VariableDeclaration::ConstDeclaration { keyword, declarations }
			}
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
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

	fn get_position(&self) -> Cow<Span> {
		match self {
			VariableDeclaration::ConstDeclaration { keyword, declarations } => {
				Cow::Owned(keyword.1.union(&declarations.last().unwrap().get_position()))
			}
			VariableDeclaration::LetDeclaration { keyword, declarations } => {
				Cow::Owned(keyword.1.union(&declarations.last().unwrap().get_position()))
			}
		}
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
	settings: &crate::ToStringSettings,
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

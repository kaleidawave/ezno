use std::borrow::Cow;

use tokenizer_lib::Token;

use crate::{
	errors::parse_lexing_error,
	parse_bracketed, to_string_bracketed,
	tokens::token_as_identifier,
	types::{type_declarations::TypeDeclaration, type_references::TypeReferenceFunctionParameters},
	ASTNode, Decorator, GenericTypeConstraint, ParseResult, ParseSettings, Span, TSXKeyword,
	TSXToken, TokenReader, TypeDefinitionModuleStatement, TypeId, TypeReference, VariableId,
};

/// A `declare var` thingy.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct DeclareVariableDeclaration {
	pub name: String,
	pub type_restriction: TypeReference,
	pub variable_id: VariableId,
	pub decorators: Vec<Decorator>,
	pub position: Span,
}

impl ASTNode for DeclareVariableDeclaration {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare(reader, state, settings, Some(start_span), Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if settings.0.include_types {
			buf.push_str("declare var ");
			buf.push_str(&self.name);
			buf.push_str(": ");
			self.type_restriction.to_string_from_buffer(buf, settings, depth);
		}
	}
}

impl DeclareVariableDeclaration {
	pub fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		declare_span: Option<Span>,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let var_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::Var))?;
		let (name, _) = token_as_identifier(reader.next().unwrap(), "declare variable name")?;
		reader.expect_next(TSXToken::Colon)?;
		let type_restriction = TypeReference::from_reader(reader, state, settings)?;
		let position = declare_span.unwrap_or(var_pos).union(&type_restriction.get_position());
		Ok(Self { name, type_restriction, variable_id: VariableId::new(), position, decorators })
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct DeclareFunctionDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub parameters: TypeReferenceFunctionParameters,
	pub return_type: Option<TypeReference>,
	pub variable_id: VariableId,
	pub decorators: Vec<Decorator>,
	pub position: Span,
}

impl ASTNode for DeclareFunctionDeclaration {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare_with_decorators(reader, state, settings, Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if settings.0.include_types {
			buf.push_str("declare function ");
			buf.push_str(self.name.as_str());
			if let Some(type_parameters) = &self.type_parameters {
				to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth);
			}
			self.parameters.to_string_from_buffer(buf, settings, depth);
			if let Some(return_type) = &self.return_type {
				buf.push_str(": ");
				return_type.to_string_from_buffer(buf, settings, depth)
			}
		}
	}
}

impl DeclareFunctionDeclaration {
	pub fn from_reader_sub_declare_with_decorators(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let start_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::Function))?;
		let (name, _) = token_as_identifier(
			reader.next().ok_or_else(parse_lexing_error)?,
			"declare function name",
		)?;
		let type_parameters =
			if reader.conditional_next(|tok| *tok == TSXToken::OpenChevron).is_some() {
				Some(parse_bracketed(reader, state, settings, None, TSXToken::CloseChevron)?.0)
			} else {
				None
			};
		let parameters = TypeReferenceFunctionParameters::from_reader(reader, state, settings)?;
		let return_type = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
			reader.next();
			let type_reference = TypeReference::from_reader(reader, state, settings)?;
			Some(type_reference)
		} else {
			None
		};
		let position = start_pos.union(
			&return_type
				.as_ref()
				.map(ASTNode::get_position)
				.unwrap_or(Cow::Borrowed(&parameters.position)),
		);
		Ok(Self {
			name,
			type_parameters,
			parameters,
			return_type,
			decorators,
			variable_id: VariableId::new(),
			position,
		})
	}
}

/// e.g. `type NumberArray = Array<number>`
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct TypeAlias {
	pub type_name: TypeDeclaration,
	pub type_expression: TypeReference,
	pub type_id: TypeId,
	position: Span,
}

impl ASTNode for TypeAlias {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Type))?;
		let type_name = TypeDeclaration::from_reader(reader, state, settings)?;
		reader.expect_next(TSXToken::Assign)?;
		let type_expression = TypeReference::from_reader(reader, state, settings)?;
		let position = start.union(&type_expression.get_position());
		Ok(Self { type_name, type_expression, type_id: TypeId::new(), position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		if settings.0.include_types {
			buf.push_str("type ");
			self.type_name.to_string_from_buffer(buf, settings, depth);
			buf.push_str(" = ");
			self.type_expression.to_string_from_buffer(buf, settings, depth);
		} else {
			panic!()
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Namespace(String, Vec<TypeDefinitionModuleStatement>);

impl ASTNode for Namespace {
	fn get_position(&self) -> Cow<Span> {
		unimplemented!()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Namespace))?;
		let (namespace_name, _) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "namespace name")?;
		reader.expect_next(TSXToken::OpenBrace)?;
		let mut declarations = Vec::new();
		while let Some(token) = reader.peek() {
			if let Token(TSXToken::CloseBrace, _) = token {
				break;
			}
			declarations.push(TypeDefinitionModuleStatement::from_reader(reader, state, settings)?);
			if matches!(reader.peek(), Some(Token(TSXToken::SemiColon, _))) {
				reader.next();
			}
		}
		reader.next();
		Ok(Self(namespace_name, declarations))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
		_depth: u8,
	) {
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclareClassDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub extends: Option<TypeReference>,
	pub type_id: TypeId, // TODO:
	                     // members: Vec<DeclareClassMember>
}

impl ASTNode for DeclareClassDeclaration {
	fn get_position(&self) -> Cow<Span> {
		todo!()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare(reader, state, settings)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringSettingsAndData,
		_depth: u8,
	) {
		todo!()
	}
}

impl DeclareClassDeclaration {
	pub(crate) fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Class))?;
		let (name, _) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "class")?;
		let extends = if let Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) = reader.peek()
		{
			reader.next();
			Some(TypeReference::from_reader(reader, state, settings)?)
		} else {
			None
		};
		reader.expect_next(TSXToken::OpenBrace)?;
		// TODO members
		reader.expect_next(TSXToken::CloseBrace)?;
		Ok(Self { name, extends, type_parameters: None, type_id: TypeId::new() })
	}
}

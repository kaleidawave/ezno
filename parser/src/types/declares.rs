use tokenizer_lib::{sized_tokens::TokenStart, Token};

use crate::{
	errors::parse_lexing_error, parse_bracketed, to_string_bracketed, tokens::token_as_identifier,
	types::type_annotations::TypeAnnotationFunctionParameters, ASTNode, Decorator,
	GenericTypeConstraint, ParseOptions, ParseResult, Span, TSXKeyword, TSXToken, TokenReader,
	TypeAnnotation,
};

use super::AnnotationPerforms;

/// A `declare var` thingy.
#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct DeclareVariableDeclaration {
	pub name: String,
	pub type_restriction: TypeAnnotation,
	pub decorators: Vec<Decorator>,
	pub position: Span,
}

impl ASTNode for DeclareVariableDeclaration {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare(reader, state, settings, Some(start_span), Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if settings.include_types {
			buf.push_str("declare var ");
			buf.push_str(&self.name);
			buf.push_str(": ");
			self.type_restriction.to_string_from_buffer(buf, settings, depth);
		}
	}
}

impl DeclareVariableDeclaration {
	pub fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		start: Option<TokenStart>,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let var_start = reader.expect_next(TSXToken::Keyword(TSXKeyword::Var))?;
		let (name, _) = token_as_identifier(reader.next().unwrap(), "declare variable name")?;
		reader.expect_next(TSXToken::Colon)?;
		let type_restriction = TypeAnnotation::from_reader(reader, state, settings)?;
		let position = start.unwrap_or(var_start).union(type_restriction.get_position());
		Ok(Self { name, type_restriction, position, decorators })
	}
}

#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct DeclareFunctionDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub parameters: TypeAnnotationFunctionParameters,
	pub return_type: Option<TypeAnnotation>,
	#[cfg(feature = "extras")]
	pub performs: Option<AnnotationPerforms>,
	pub decorators: Vec<Decorator>,
	pub position: Span,
}

impl ASTNode for DeclareFunctionDeclaration {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare_with_decorators(reader, state, settings, Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if settings.include_types {
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
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
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
		let parameters = TypeAnnotationFunctionParameters::from_reader(reader, state, settings)?;
		let return_type = if let Some(Token(TSXToken::Colon, _)) = reader.peek() {
			reader.next();
			let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
			Some(type_annotation)
		} else {
			None
		};

		#[cfg(feature = "extras")]
		let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) = reader.peek() {
			Some(AnnotationPerforms::from_reader(reader, state, settings)?)
		} else {
			None
		};

		let position = start_pos
			.union(return_type.as_ref().map(ASTNode::get_position).unwrap_or(&parameters.position));

		Ok(Self {
			name,
			type_parameters,
			parameters,
			return_type,
			decorators,
			position,
			#[cfg(feature = "extras")]
			performs,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclareClassDeclaration {
	pub name: String,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	pub extends: Option<TypeAnnotation>,
	// members: Vec<DeclareClassMember>
}

impl ASTNode for DeclareClassDeclaration {
	fn get_position(&self) -> &Span {
		todo!()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Declare))?;
		Self::from_reader_sub_declare(reader, state, settings)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		todo!()
	}
}

impl DeclareClassDeclaration {
	pub(crate) fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		reader.expect_next(TSXToken::Keyword(TSXKeyword::Class))?;
		let (name, _) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "class")?;
		let extends = if let Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) = reader.peek()
		{
			reader.next();
			Some(TypeAnnotation::from_reader(reader, state, settings)?)
		} else {
			None
		};
		reader.expect_next(TSXToken::OpenBrace)?;
		// TODO members
		reader.expect_next(TSXToken::CloseBrace)?;
		Ok(Self { name, extends, type_parameters: None })
	}
}

use tokenizer_lib::{sized_tokens::TokenStart, Token};

use crate::{
	declarations::VariableDeclarationItem, errors::parse_lexing_error, parse_bracketed,
	to_string_bracketed, tokens::token_as_identifier,
	types::type_annotations::TypeAnnotationFunctionParameters, ASTNode, Decorator,
	GenericTypeConstraint, ParseOptions, ParseResult, Span, TSXKeyword, TSXToken, TokenReader,
	TypeAnnotation, VariableKeyword,
};

/// A `declare var/let/const` thingy.
#[derive(Debug, Clone, PartialEq, Eq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub struct DeclareVariableDeclaration {
	pub keyword: VariableKeyword,
	/// TODO expressions advised against, but still parse
	pub declarations: Vec<VariableDeclarationItem<Option<crate::Expression>>>,
	pub position: Span,
	pub decorators: Vec<Decorator>,
}

impl ASTNode for DeclareVariableDeclaration {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::Declare)?;
		Self::from_reader_sub_declare(reader, state, options, Some(start), Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		if options.include_types {
			buf.push_str("declare ");
			buf.push_str(self.keyword.as_str());
			crate::declarations::variable::declarations_to_string(
				&self.declarations,
				buf,
				options,
				depth,
			);
		}
	}
}

impl DeclareVariableDeclaration {
	pub fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: Option<TokenStart>,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		let start = start.unwrap_or(token.1);
		let keyword = VariableKeyword::from_reader(token)?;
		let mut declarations = Vec::new();
		loop {
			let value = VariableDeclarationItem::from_reader(reader, state, options)?;
			declarations.push(value);
			if let Some(Token(TSXToken::Comma, _)) = reader.peek() {
				reader.next();
			} else {
				break;
			}
		}

		let position = start.union(declarations.last().unwrap().get_position());

		Ok(DeclareVariableDeclaration { keyword, declarations, position, decorators })
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
	pub performs: Option<super::AnnotationPerforms>,
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
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let _ = state.expect_keyword(reader, TSXKeyword::Declare)?;
		Self::from_reader_sub_declare_with_decorators(reader, state, options, Vec::new())
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		depth: u8,
	) {
		if options.include_types {
			buf.push_str("declare function ");
			buf.push_str(self.name.as_str());
			if let Some(type_parameters) = &self.type_parameters {
				to_string_bracketed(type_parameters, ('<', '>'), buf, options, depth);
			}
			self.parameters.to_string_from_buffer(buf, options, depth);
			if let Some(return_type) = &self.return_type {
				buf.push_str(": ");
				return_type.to_string_from_buffer(buf, options, depth);
			}
		}
	}
}

impl DeclareFunctionDeclaration {
	pub fn from_reader_sub_declare_with_decorators(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		decorators: Vec<Decorator>,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::Function)?;
		let (name, _) = token_as_identifier(
			reader.next().ok_or_else(parse_lexing_error)?,
			"declare function name",
		)?;
		let type_parameters =
			if reader.conditional_next(|tok| *tok == TSXToken::OpenChevron).is_some() {
				Some(parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)?.0)
			} else {
				None
			};
		let parameters = TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;
		let return_type = if reader.conditional_next(|tok| matches!(tok, TSXToken::Colon)).is_some()
		{
			let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
			Some(type_annotation)
		} else {
			None
		};

		#[cfg(feature = "extras")]
		let performs = if let Some(Token(TSXToken::Keyword(TSXKeyword::Performs), _)) = reader.peek() {
			Some(super::AnnotationPerforms::from_reader(reader, state, options)?)
		} else {
			None
		};

		let position =
			start.union(return_type.as_ref().map_or(&parameters.position, ASTNode::get_position));

		Ok(Self {
			name,
			type_parameters,
			parameters,
			return_type,
			#[cfg(feature = "extras")]
			performs,
			decorators,
			position,
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
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let _ = state.expect_keyword(reader, TSXKeyword::Declare)?;
		Self::from_reader_sub_declare(reader, state, options)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_options: &crate::ToStringOptions,
		_depth: u8,
	) {
		todo!()
	}
}

impl DeclareClassDeclaration {
	pub(crate) fn from_reader_sub_declare(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let _ = state.expect_keyword(reader, TSXKeyword::Class)?;
		let (name, _) =
			token_as_identifier(reader.next().ok_or_else(parse_lexing_error)?, "class")?;
		let extends = if let Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) = reader.peek()
		{
			reader.next();
			Some(TypeAnnotation::from_reader(reader, state, options)?)
		} else {
			None
		};
		reader.expect_next(TSXToken::OpenBrace)?;
		// TODO members
		reader.expect_next(TSXToken::CloseBrace)?;
		Ok(Self { name, extends, type_parameters: None })
	}
}

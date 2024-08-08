mod class_member;

use std::fmt::Debug;

use crate::{
	derive_ASTNode, throw_unexpected_token_with_token, to_string_bracketed, Expression,
	ParseErrors, VariableIdentifier,
};
pub use class_member::*;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	extensions::decorators::Decorated, visiting::Visitable, ASTNode, ExpressionOrStatementPosition,
	ParseOptions, ParseResult, Span, TSXKeyword, TSXToken, TypeAnnotation, TypeParameter,
	VisitOptions,
};
use tokenizer_lib::{
	sized_tokens::{TokenReaderWithTokenEnds, TokenStart},
	Token, TokenReader,
};

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ClassDeclaration<T: ExpressionOrStatementPosition> {
	pub name: T,
	pub type_parameters: Option<Vec<TypeParameter>>,
	pub extends: Option<Box<Expression>>,
	pub implements: Option<Vec<TypeAnnotation>>,
	pub members: Vec<Decorated<ClassMember>>,
	pub position: Span,
}

impl<U: ExpressionOrStatementPosition + Debug + PartialEq + Clone + 'static> ASTNode
	for ClassDeclaration<U>
{
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = state.expect_keyword(reader, TSXKeyword::Class)?;
		Self::from_reader_sub_class_keyword(reader, state, options, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.to_string_from_buffer(buf, options, local);
	}

	fn get_position(&self) -> Span {
		self.position
	}
}

impl<U: ExpressionOrStatementPosition> ClassDeclaration<U> {
	pub(crate) fn from_reader_sub_class_keyword(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let name = U::from_reader(reader, state, options)?;

		if let Some(VariableIdentifier::Standard(name, pos)) = name.as_option_variable_identifier()
		{
			if let "extends" = name.as_str() {
				return Err(crate::ParseError::new(ParseErrors::ExpectedIdentifier, *pos));
			}
		}

		let type_parameters = reader
			.conditional_next(|token| *token == TSXToken::OpenChevron)
			.is_some()
			.then(|| {
				crate::parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
					.map(|(params, _, _)| params)
			})
			.transpose()?;

		let extends = if reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Extends)))
			.is_some()
		{
			Some(Expression::from_reader(reader, state, options)?.into())
		} else {
			None
		};

		let implements = if reader
			.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Implements)))
			.is_some()
		{
			let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
			let mut implements = vec![type_annotation];
			if reader.conditional_next(|t| matches!(t, TSXToken::Comma)).is_some() {
				loop {
					implements.push(TypeAnnotation::from_reader(reader, state, options)?);
					match reader.peek() {
						Some(Token(TSXToken::Comma, _)) => {
							reader.next();
						}
						Some(Token(TSXToken::OpenBrace, _)) | None => break,
						_ => {
							return throw_unexpected_token_with_token(
								reader.next().unwrap(),
								&[TSXToken::Comma, TSXToken::OpenBrace],
							)
						}
					}
				}
			}
			Some(implements)
		} else {
			None
		};

		reader.expect_next(TSXToken::OpenBrace)?;
		let mut members: Vec<Decorated<ClassMember>> = Vec::new();
		loop {
			if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
				break;
			}
			let value = Decorated::<ClassMember>::from_reader(reader, state, options)?;
			members.push(value);

			if let Some(Token(TSXToken::SemiColon, _)) = reader.peek() {
				reader.next();
			}
		}
		let position = start.union(reader.expect_next_get_end(TSXToken::CloseBrace)?);

		Ok(ClassDeclaration { name, type_parameters, extends, implements, members, position })
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("class ");
		if let Some(name) = self.name.as_option_str() {
			buf.push_str(name);
		}
		if let Some(type_parameters) = &self.type_parameters {
			to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
		}
		if let Some(extends) = &self.extends {
			buf.push_str(" extends ");
			extends.to_string_from_buffer(buf, options, local);
		}
		options.push_gap_optionally(buf);
		buf.push('{');
		for (at_end, member) in self.members.iter().endiate() {
			if options.pretty {
				buf.push_new_line();
				options.add_indent(local.depth + 1, buf);
			}
			member.to_string_from_buffer(buf, options, local);
			if !options.pretty && !at_end {
				buf.push(';');
			}
		}
		if options.pretty && !self.members.is_empty() {
			buf.push_new_line();
		}
		buf.push('}');
	}
}

impl<T: ExpressionOrStatementPosition> Visitable for ClassDeclaration<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable(
			&crate::visiting::ImmutableVariableOrProperty::ClassName(
				self.name.as_option_variable_identifier(),
			),
			data,
			chain,
		);
		self.extends.visit(visitors, data, options, chain);
		self.members.visit(visitors, data, options, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		visitors.visit_variable_mut(
			&mut crate::visiting::MutableVariableOrProperty::ClassName(
				self.name.as_option_variable_identifier_mut(),
			),
			data,
			chain,
		);
		self.extends.visit_mut(visitors, data, options, chain);
		self.members.visit_mut(visitors, data, options, chain);
	}
}

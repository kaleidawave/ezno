mod class_member;

use std::{borrow::Cow, fmt::Debug};

use crate::{to_string_bracketed, tsx_keywords};
pub use class_member::*;
use iterator_endiate::EndiateIteratorExt;

use crate::{
	extensions::decorators::Decorated, visiting::Visitable, ASTNode, ExpressionOrStatementPosition,
	GenericTypeConstraint, Keyword, ParseResult, ParseSettings, Span, TSXKeyword, TSXToken, TypeId,
	TypeReference, VariableId, VisitSettings,
};
use tokenizer_lib::{Token, TokenReader};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDeclaration<T: ExpressionOrStatementPosition> {
	pub class_keyword: Keyword<tsx_keywords::Class>,
	pub name: T::Name,
	pub type_parameters: Option<Vec<GenericTypeConstraint>>,
	/// TODO shouldn't be type reference
	pub extends: Option<TypeReference>,
	pub members: Vec<Decorated<ClassMember>>,
	/// The [TypeId] is the type of the instance the class defines
	pub type_id: TypeId,
	/// The [VariableId] is for `SomeClass.constructor` and possible static properties etc
	pub variable_id: VariableId,
	pub position: Span,
}

#[cfg(feature = "self-rust-tokenize")]
impl<T: ExpressionOrStatementPosition> self_rust_tokenize::SelfRustTokenize
	for ClassDeclaration<T>
{
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!()
	}
}

impl<U: ExpressionOrStatementPosition + Debug + PartialEq + Eq + Clone + 'static> ASTNode
	for ClassDeclaration<U>
{
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let class_keyword_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::Class))?;
		let class_keyword = Keyword::new(class_keyword_pos);
		Self::from_reader_sub_class_keyword(reader, state, settings, class_keyword)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		self.to_string_from_buffer(buf, settings, depth)
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}
}

impl<U: ExpressionOrStatementPosition> ClassDeclaration<U> {
	pub(crate) fn from_reader_sub_class_keyword(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
		class_keyword: Keyword<tsx_keywords::Class>,
	) -> ParseResult<Self> {
		let (name, type_parameters) = U::from_reader(reader, state, settings)?;
		let extends = match reader.peek() {
			Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) => {
				reader.next();
				Some(TypeReference::from_reader(reader, state, settings)?)
			}
			_ => None,
		};
		reader.expect_next(TSXToken::OpenBrace)?;
		let mut members: Vec<Decorated<ClassMember>> = Vec::new();
		loop {
			if let Some(Token(TSXToken::CloseBrace, _)) = reader.peek() {
				break;
			}
			let value = Decorated::<ClassMember>::from_reader(reader, state, settings)?;
			members.push(value);

			if let Some(Token(TSXToken::SemiColon, _)) = reader.peek() {
				reader.next();
			}
		}
		let position = class_keyword.1.union(&reader.expect_next(TSXToken::CloseBrace)?);
		Ok(ClassDeclaration {
			class_keyword,
			name,
			type_id: TypeId::new(),
			variable_id: VariableId::new(),
			extends,
			members,
			type_parameters,
			position,
		})
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettings,
		depth: u8,
	) {
		buf.push_str("class ");
		buf.push_str(U::as_option_str(&self.name).unwrap_or_default());
		if let Some(type_parameters) = &self.type_parameters {
			to_string_bracketed(type_parameters, ('<', '>'), buf, settings, depth);
		}
		if let Some(extends) = &self.extends {
			buf.push_str(" extends ");
			extends.to_string_from_buffer(buf, settings, depth);
		}
		settings.add_gap(buf);
		buf.push('{');
		for (at_end, member) in self.members.iter().endiate() {
			if settings.pretty {
				buf.push_new_line();
				settings.add_indent(depth + 1, buf);
			}
			member.to_string_from_buffer(buf, settings, depth);
			if !settings.pretty && !at_end {
				buf.push(';');
			}
		}
		if settings.pretty && !self.members.is_empty() {
			buf.push_new_line();
		}
		buf.push('}');
	}

	pub fn get_variable_id(&self) -> VariableId {
		self.variable_id
	}
}

impl<T: ExpressionOrStatementPosition> Visitable for ClassDeclaration<T> {
	fn visit<TData>(
		&self,
		_visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_data: &mut TData,
		_settings: &VisitSettings,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn visit_mut<TData>(
		&mut self,
		_visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_data: &mut TData,
		_settings: &VisitSettings,
		_chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}
}

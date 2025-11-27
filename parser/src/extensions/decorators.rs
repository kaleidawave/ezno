use get_field_by_type::GetFieldByType;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{ASTNode, Expression, ParseResult, Visitable, derive_ASTNode};

/// The [decorators](https://github.com/tc39/proposal-decorators) proposal.
///
/// Decorators are expressions. Also allowing `@(x + 2)`
#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub struct Decorator(pub Expression);

impl ASTNode for Decorator {
	fn get_position(&self) -> Span {
		self.0.get_position()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		// TODO modify position? or new
		let _start = reader.get_start();
		reader.expect('@')?;
		dbg!(reader.get_current());
		let expression = Expression::from_reader_with_precedence(
			reader,
			crate::expressions::precedence::FUNCTION_CALL_PRECEDENCE,
		)?;
		// TODO check valid here
		Ok(Self(expression))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_decorators {
			buf.push('@');
			self.0.to_string_from_buffer(buf, options, local)
		}
	}
}

/// FUTURE under cfg if the user does not want the decorators feature, this definition could be swapped out with `type Decorated<T> = T;`
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Decorated<T> {
	pub decorators: Vec<Decorator>,
	pub on: T,
	// TODO option and on t
	pub position: Span,
}

impl<N: ASTNode> ASTNode for Decorated<N> {
	fn get_position(&self) -> Span {
		*self.get()
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader)?;
		N::from_reader(reader).map(|on| Self::new(decorators, on))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.to_string_from_buffer_just_decorators(buf, options, local);
		self.on.to_string_from_buffer(buf, options, local);
	}
}

impl<U: ASTNode> Decorated<U> {
	pub fn new_empty(on: U) -> Self {
		Self::new(Default::default(), on)
	}

	pub fn new(decorators: Vec<Decorator>, on: U) -> Self {
		let position = decorators
			.first()
			.map_or(on.get_position(), |d| d.get_position())
			.union(on.get_position());
		Self { decorators, on, position }
	}

	pub(crate) fn to_string_from_buffer_just_decorators<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_decorators {
			for decorator in &self.decorators {
				decorator.to_string_from_buffer(buf, options, local);
				if options.pretty {
					buf.push_new_line();
				} else {
					buf.push(' ');
				}
			}
		}
	}
}

pub(crate) fn decorators_from_reader(reader: &mut crate::Lexer) -> ParseResult<Vec<Decorator>> {
	let mut decorators = Vec::new();
	while reader.starts_with('@') {
		decorators.push(Decorator::from_reader(reader)?);
	}
	Ok(decorators)
}

impl<T: Visitable> Visitable for Decorated<T> {
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,

		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit(visitors, data, options, chain);
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::VisitOptions,

		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		self.on.visit_mut(visitors, data, options, chain);
	}
}

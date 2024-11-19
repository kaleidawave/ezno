use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{derive_ASTNode, ASTNode, Expression, ParseOptions, ParseResult, Visitable};

#[derive(Debug, PartialEq, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub struct Decorator {
	pub name: Vec<String>,
	pub arguments: Option<Vec<Expression>>,
	pub position: Span,
}

impl ASTNode for Decorator {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		reader.expect('@')?;
		let mut name = vec![reader.parse_identifier("decorator name").unwrap().to_owned()];
		while reader.is_operator_advance(".") {
			name.push(reader.parse_identifier("decorator name").unwrap().to_owned());
		}

		let arguments = if reader.starts_with('(') {
			let mut arguments = Vec::<_>::new();
			// TODO could we use parse_bracketed
			loop {
				if reader.starts_with(')') {
					break;
				}
				arguments.push(Expression::from_reader(reader)?);
				if !reader.is_operator_advance(",") {
					break;
				}
			}
			let _ = reader.expect(')')?;
			Some(arguments)
		} else {
			None
		};
		let position = start.union(reader.get_end());
		Ok(Self { name, arguments, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_decorators {
			buf.push('@');
			for (not_at_end, value) in self.name.iter().nendiate() {
				buf.push_str(value);
				if not_at_end {
					buf.push('.');
				}
			}
			if let Some(arguments) = &self.arguments {
				buf.push('(');
				for (at_end, argument) in arguments.iter().endiate() {
					argument.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
				buf.push(')');
			}
		}
	}
}

/// TODO under cfg if don't want this could just be `type Decorated<T> = T;`
#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, get_field_by_type::GetFieldByType)]
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

	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
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
		let position =
			decorators.first().map_or(on.get_position(), |d| d.position).union(on.get_position());
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

pub(crate) fn decorators_from_reader(
	reader: &mut crate::new::Lexer,
) -> ParseResult<Vec<Decorator>> {
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

use source_map::SourceId;

use crate::{
	ArrayDestructuringField, Expression, JSXElement, ObjectDestructuringField, PropertyKey,
	Statement, WithComment,
};

pub use ast::*;
pub use structures::*;
pub use visitors::*;
pub use visitors_mut::*;

mod ast {
	use temporary_annex::Annex;

	use crate::block::{BlockLike, BlockLikeMut};

	use super::*;

	/// Options for behavior when visiting AST.
	/// Customizable behavior is important for analysis
	pub struct VisitSettings {
		/// Visits statements in reverse,
		/// e.g
		/// ```ts
		/// const x = 2;
		/// const y = 3;
		/// ```
		/// If `reverse_statements` is true will visit the `const y = 3` statement first
		pub reverse_statements: bool,
		/// Will not visit parameters and statements in functions. This includes arrow functions
		pub visit_function_bodies: bool,
	}

	impl Default for VisitSettings {
		fn default() -> Self {
			Self { reverse_statements: false, visit_function_bodies: true }
		}
	}

	/// Whether something has visit self
	macro_rules! mark_items {
        (impl $trait:ident for $($t:ty),*) => {
            $(
                impl $trait for $t {}
            )*
        }
    }
	pub trait SelfVisitable {}
	pub trait SelfVisitableMut {}

	mark_items! {
		impl SelfVisitable for Expression, Statement, BlockLike<'_>, JSXElement, ImmutableVariableOrPropertyPart<'_>
	}

	mark_items! {
		impl SelfVisitableMut for Expression, Statement, BlockLikeMut<'_>, JSXElement, MutableVariablePart<'_>
	}

	/// For something to visitable it can visit all nested fields.
	pub trait Visitable {
		fn visit<TData>(
			&self,
			visitors: &mut (impl VisitorReceiver<TData> + ?Sized),
			data: &mut TData,
			settings: &VisitSettings,
			chain: &mut Annex<Chain>,
		);

		fn visit_mut<TData>(
			&mut self,
			visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			data: &mut TData,
			settings: &VisitSettings,
			chain: &mut Annex<Chain>,
		);
	}

	// Implementing Visitable to some structures that are commonly used in AST
	impl<T: Visitable> Visitable for Box<T> {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			Visitable::visit(&**self, v, d, s, c)
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			Visitable::visit_mut(&mut **self, v, d, s, c)
		}
	}

	impl<T: Visitable> Visitable for Vec<T> {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			self.iter().for_each(|item| item.visit(v, d, s, c));
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			self.iter_mut().for_each(|item| item.visit_mut(v, d, s, c));
		}
	}

	impl<T: Visitable> Visitable for Option<T> {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			if let Some(item) = self {
				item.visit(v, d, s, c);
			}
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			if let Some(item) = self {
				item.visit_mut(v, d, s, c);
			}
		}
	}

	impl<T: Visitable, U: Visitable> Visitable for (T, U) {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			self.0.visit(v, d, s, c);
			self.1.visit(v, d, s, c);
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitSettings,
			c: &mut Annex<Chain>,
		) {
			self.0.visit_mut(v, d, s, c);
			self.1.visit_mut(v, d, s, c);
		}
	}

	macro_rules! create_blank_visiting_implementations {
        ($($T:ty),*) => {
            $(
                impl Visitable for $T {
                    fn visit<TData>(
                        &self,
                        _visitors: &mut (impl VisitorReceiver<TData> + ?Sized),
                        _data: &mut TData,
                        _settings: &VisitSettings,
                        _chain: &mut Annex<Chain>,
                    ) {}

                    fn visit_mut<TData>(
                        &mut self,
                        _visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
                        _data: &mut TData,
                        _settings: &VisitSettings,
                        _chain: &mut Annex<Chain>,
                    ) {}
                }
            )*
        }
    }

	// Create a bunch of blank implementations for data types that do not have
	// any AST nested / aren't important for visiting.
	// TODO propertyKey should be visited
	create_blank_visiting_implementations![
		(),
		bool,
		isize,
		usize,
		i8,
		u8,
		i16,
		u16,
		i32,
		u32,
		i64,
		u64,
		i128,
		u128,
		f32,
		f64,
		char,
		String,
		Box<str>,
		std::rc::Rc<str>,
		std::path::Path,
		std::path::PathBuf,
		source_map::Span,
		crate::TypeAnnotation,
		crate::NumberStructure,
		crate::operators::BinaryOperator,
		crate::operators::BinaryAssignmentOperator,
		crate::operators::UnaryOperator,
		crate::operators::UnaryPrefixAssignmentOperator,
		crate::operators::UnaryPostfixAssignmentOperator,
		crate::types::InterfaceDeclaration,
		crate::types::type_alias::TypeAlias,
		crate::types::declares::DeclareFunctionDeclaration,
		crate::types::declares::DeclareVariableDeclaration,
		crate::VariableIdentifier,
		crate::PropertyReference,
		crate::Quoted,
		crate::PropertyKey<crate::property_key::AlwaysPublic>,
		crate::PropertyKey<crate::property_key::PublicOrPrivate>
	];
}

/// These are structures used when visiting AST
mod structures {

	use crate::{
		property_key::{AlwaysPublic, PublicOrPrivate},
		VariableFieldInSourceCode,
	};

	use super::*;
	use source_map::Span;
	use temporary_annex::{Annex, Annexable};

	#[derive(Debug, Clone)]
	pub enum ChainVariable {
		Module(SourceId),
		Function(Span),
		Block(Span),
	}

	/// The current location in the AST
	#[derive(Debug, Clone)]
	pub struct Chain(Vec<ChainVariable>);

	impl Chain {
		pub fn new() -> Self {
			Self(Vec::with_capacity(10))
		}

		pub fn new_with_initial(initial: ChainVariable) -> Self {
			let mut buf = Vec::with_capacity(10);
			buf.push(initial);
			Self(buf)
		}

		pub fn len(&self) -> usize {
			self.0.len()
		}

		pub fn truncate(&mut self, to_size: usize) {
			self.0.truncate(to_size)
		}

		/// TODO remove
		pub fn get_chain(&self) -> &[ChainVariable] {
			&self.0
		}

		pub fn get_module(&self) -> SourceId {
			if let ChainVariable::Module(source) = self.0.first().unwrap() {
				source.to_owned()
			} else {
				SourceId::NULL
			}
		}

		// TODO get function root. Aka last thing before in top level scope or
	}

	impl Annexable for Chain {
		type NewItem = ChainVariable;

		fn push_annex(&mut self, item: Self::NewItem) -> Annex<Self>
		where
			Self: Sized,
		{
			self.0.push(item);
			Annex::new(self)
		}

		fn revert_annex(&mut self) {
			self.0.pop();
		}
	}

	/// TODO these may go
	#[derive(Debug)]
	pub enum MutableVariablePart<'a> {
		VariableFieldName(&'a mut String),
		// TODO these should maybe only be the spread variables
		ArrayDestructuringMember(&'a mut ArrayDestructuringField<VariableFieldInSourceCode>),
		ObjectDestructuringMember(
			&'a mut WithComment<ObjectDestructuringField<VariableFieldInSourceCode>>,
		),
		ClassName(Option<&'a mut String>),
		FunctionName(&'a mut String),
		ClassPropertyKey(&'a mut WithComment<PropertyKey<PublicOrPrivate>>),
		ObjectPropertyKey(&'a mut WithComment<PropertyKey<AlwaysPublic>>),
	}

	/// TODO these may go
	#[derive(Debug)]
	pub enum ImmutableVariableOrPropertyPart<'a> {
		// TODO maybe WithComment on some of these
		VariableFieldName(&'a str, &'a Span),
		// TODO these should maybe only be the spread variables
		ArrayDestructuringMember(&'a ArrayDestructuringField<VariableFieldInSourceCode>),
		ObjectDestructuringMember(
			&'a WithComment<ObjectDestructuringField<VariableFieldInSourceCode>>,
		),
		ClassName(Option<&'a str>, &'a Span),
		FunctionName(&'a str, &'a Span),
		ClassPropertyKey(&'a WithComment<PropertyKey<PublicOrPrivate>>),
		ObjectPropertyKey(&'a WithComment<PropertyKey<AlwaysPublic>>),
	}

	impl<'a> ImmutableVariableOrPropertyPart<'a> {
		pub fn get_name(&self) -> Option<&'a str> {
			match self {
				ImmutableVariableOrPropertyPart::FunctionName(name, _)
				| ImmutableVariableOrPropertyPart::VariableFieldName(name, _) => Some(name),
				ImmutableVariableOrPropertyPart::ArrayDestructuringMember(_)
				| ImmutableVariableOrPropertyPart::ObjectDestructuringMember(_) => None,
				ImmutableVariableOrPropertyPart::ClassName(name, _) => *name,
				ImmutableVariableOrPropertyPart::ObjectPropertyKey(property) => {
					match property.get_ast_ref() {
						PropertyKey::Ident(ident, _, _) | PropertyKey::StringLiteral(ident, _) => {
							Some(ident.as_str())
						}
						PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => None,
					}
				}
				ImmutableVariableOrPropertyPart::ClassPropertyKey(property) => {
					match property.get_ast_ref() {
						PropertyKey::Ident(ident, _, _) | PropertyKey::StringLiteral(ident, _) => {
							Some(ident.as_str())
						}
						PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => None,
					}
				}
			}
		}

		pub fn get_position(&self) -> &Span {
			use crate::ASTNode;
			match self {
				ImmutableVariableOrPropertyPart::FunctionName(_, pos)
				| ImmutableVariableOrPropertyPart::ClassName(_, pos)
				| ImmutableVariableOrPropertyPart::VariableFieldName(_, pos) => pos,
				ImmutableVariableOrPropertyPart::ArrayDestructuringMember(member) => {
					member.get_position()
				}
				ImmutableVariableOrPropertyPart::ObjectDestructuringMember(member) => {
					member.get_position()
				}
				ImmutableVariableOrPropertyPart::ClassPropertyKey(property_key) => {
					property_key.get_position()
				}
				ImmutableVariableOrPropertyPart::ObjectPropertyKey(property_key) => {
					property_key.get_position()
				}
			}
		}
	}
}

mod visitors {
	use super::*;
	use crate::{block::BlockLike, TSXKeyword};
	use source_map::Span;

	/// A visitor over something which is hooked/is SelfVisitable with some Data
	pub trait Visitor<Item: SelfVisitable, Data> {
		fn visit(&mut self, item: &Item, data: &mut Data, chain: &Chain);
	}

	/// These are a receiver traits of the visitor
	#[allow(unused_variables)]
	pub trait VisitorReceiver<T> {
		fn visit_expression(&mut self, expression: &Expression, data: &mut T, chain: &Chain) {}

		fn visit_statement(&mut self, statement: &Statement, data: &mut T, chain: &Chain) {}

		fn visit_jsx_element(&mut self, element: &JSXElement, data: &mut T, chain: &Chain) {}

		fn visit_variable(
			&mut self,
			variable: &ImmutableVariableOrPropertyPart,
			data: &mut T,
			chain: &Chain,
		) {
		}

		fn visit_block(&mut self, block: &BlockLike, data: &mut T, chain: &Chain) {}

		fn visit_keyword(&mut self, keyword: &(TSXKeyword, &Span), data: &mut T, chain: &Chain) {}
	}

	// impl<T, U: VisitorReceiver<T>> VisitorReceiver<T> for [U] {
	// 	fn visit_expression(
	// 		&mut self,
	// 		expression: &Expression,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut()
	// 			.for_each(|visitor| visitor.visit_expression(expression, data, chain));
	// 	}

	// 	fn visit_statement(
	// 		&mut self,
	// 		statement: &Statement,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut()
	// 			.for_each(|visitor| visitor.visit_statement(statement, data, chain));
	// 	}

	// 	fn visit_jsx_element(
	// 		&mut self,
	// 		element: &JSXElement,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut()
	// 			.for_each(|visitor| visitor.visit_jsx_element(element, data, chain));
	// 	}

	// 	fn visit_variable(
	// 		&mut self,
	// 		variable: &ImmutableVariableOrPropertyPart,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut()
	// 			.for_each(|visitor| visitor.visit_variable(variable, data,  chain));
	// 	}

	// 	fn visit_block(
	// 		&mut self,
	// 		block: &BlockLike,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut().for_each(|visitor| visitor.visit_block(block, data,  chain));
	// 	}

	// 	fn visit_keyword(
	// 		&mut self,
	// 		_keyword: &(TSXKeyword, &Span),
	// 		_data: &mut T,
	// 		_chain: &Chain,
	// 	) {
	// 	}
	// }

	impl<T> VisitorReceiver<T> for Visitors<T> {
		fn visit_expression(&mut self, expression: &Expression, data: &mut T, chain: &Chain) {
			self.expression_visitors.iter_mut().for_each(|vis| vis.visit(expression, data, chain));
		}

		fn visit_statement(&mut self, statement: &Statement, data: &mut T, chain: &Chain) {
			self.statement_visitors.iter_mut().for_each(|vis| vis.visit(statement, data, chain));
		}

		fn visit_jsx_element(&mut self, jsx_element: &JSXElement, data: &mut T, chain: &Chain) {
			self.jsx_element_visitors
				.iter_mut()
				.for_each(|vis| vis.visit(jsx_element, data, chain));
		}

		fn visit_variable(
			&mut self,
			variable: &ImmutableVariableOrPropertyPart,
			data: &mut T,
			chain: &Chain,
		) {
			self.variable_visitors.iter_mut().for_each(|vis| vis.visit(variable, data, chain))
		}

		fn visit_block(&mut self, block: &BlockLike, data: &mut T, chain: &Chain) {
			self.block_visitors.iter_mut().for_each(|vis| vis.visit(block, data, chain))
		}

		fn visit_keyword(&mut self, _keyword: &(TSXKeyword, &Span), _data: &mut T, _chain: &Chain) {
		}
	}

	#[derive(Default)]
	pub struct Visitors<T> {
		pub expression_visitors: Vec<Box<dyn Visitor<Expression, T>>>,
		pub statement_visitors: Vec<Box<dyn Visitor<Statement, T>>>,
		pub jsx_element_visitors: Vec<Box<dyn Visitor<JSXElement, T>>>,
		pub variable_visitors:
			Vec<Box<dyn for<'a> Visitor<ImmutableVariableOrPropertyPart<'a>, T>>>,
		pub block_visitors: Vec<Box<dyn for<'a> Visitor<BlockLike<'a>, T>>>,
	}

	// Implementors for functions
	// impl<T, U> Visitor<Expression, T> for U
	// where
	// 	U: Fn(&Expression, &mut T),
	// {
	// 	fn visit(&mut self, expression: &Expression, data: &mut T, _chain: &Chain) {
	// 		(self)(expression, data)
	// 	}
	// }

	// impl<T, U> Visitor<Statement, T> for U
	// where
	// 	U: Fn(&Statement, &mut T),
	// {
	// 	fn visit(&mut self, statement: &Statement, data: &mut T, _chain: &Chain) {
	// 		(self)(statement, data)
	// 	}
	// }

	// impl<'a, T, U> Visitor<ImmutableVariableOrPropertyPart<'a>, T> for U
	// where
	// 	U: Fn(&ImmutableVariableOrPropertyPart, &mut T),
	// {
	// 	fn visit(
	// 		&mut self,
	// 		variable: &ImmutableVariableOrPropertyPart<'a>,
	// 		data: &mut T,
	// 		_chain: &Chain,
	// 	) {
	// 		(self)(variable, data)
	// 	}
	// }
}

mod visitors_mut {
	use crate::block::BlockLikeMut;

	use super::*;

	/// A visitor over something which is hooked/is SelfVisitable with some Data
	pub trait VisitorMut<Item: SelfVisitableMut, Data> {
		fn visit_mut(&mut self, item: &mut Item, data: &mut Data, chain: &Chain);
	}

	/// These are a receiver traits of the visitor
	#[allow(unused_variables)]
	pub trait VisitorMutReceiver<T> {
		fn visit_expression_mut(
			&mut self,
			expression: &mut Expression,
			data: &mut T,
			chain: &Chain,
		) {
		}

		fn visit_statement_mut(&mut self, statement: &mut Statement, data: &mut T, chain: &Chain) {}

		fn visit_jsx_element_mut(&mut self, element: &mut JSXElement, data: &mut T, chain: &Chain) {
		}

		fn visit_variable_mut(
			&mut self,
			variable: &mut MutableVariablePart,
			data: &mut T,
			chain: &Chain,
		) {
		}

		fn visit_block_mut(&mut self, block: &mut BlockLikeMut, data: &mut T, chain: &Chain) {}
	}

	#[derive(Default)]
	pub struct VisitorsMut<T> {
		pub expression_visitors_mut: Vec<Box<dyn VisitorMut<Expression, T>>>,
		pub statement_visitors_mut: Vec<Box<dyn VisitorMut<Statement, T>>>,
		pub jsx_element_visitors_mut: Vec<Box<dyn VisitorMut<JSXElement, T>>>,
		pub variable_visitors_mut: Vec<Box<dyn for<'a> VisitorMut<MutableVariablePart<'a>, T>>>,
		pub block_visitors_mut: Vec<Box<dyn for<'a> VisitorMut<BlockLikeMut<'a>, T>>>,
	}

	// impl<T, U: VisitorMutReceiver<T>> VisitorMutReceiver<T> for [U] {
	// 	fn visit_expression_mut(
	// 		&mut self,
	// 		expression: &mut Expression,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut().for_each(|vis| vis.visit_expression_mut(expression, data, chain));
	// 	}

	// 	fn visit_statement_mut(&mut self, statement: &mut Statement, data: &mut T, chain: &Chain) {
	// 		self.iter_mut().for_each(|vis| vis.visit_statement_mut(statement, data, chain));
	// 	}

	// 	fn visit_jsx_element_mut(&mut self, element: &mut JSXElement, data: &mut T, chain: &Chain) {
	// 		self.iter_mut().for_each(|vis| vis.visit_jsx_element_mut(element, data, chain));
	// 	}

	// 	fn visit_variable_mut(
	// 		&mut self,
	// 		variable: &mut MutableVariablePart,
	// 		data: &mut T,
	// 		chain: &Chain,
	// 	) {
	// 		self.iter_mut().for_each(|vis| vis.visit_variable_mut(variable, data, chain));
	// 	}

	// 	fn visit_block_mut(&mut self, block: &mut BlockLikeMut, data: &mut T, chain: &Chain) {
	// 		self.iter_mut().for_each(|vis| vis.visit_block_mut(block, data, chain));
	// 	}
	// }

	impl<T> VisitorMutReceiver<T> for VisitorsMut<T> {
		fn visit_expression_mut(
			&mut self,
			expression: &mut Expression,
			data: &mut T,
			chain: &Chain,
		) {
			self.expression_visitors_mut
				.iter_mut()
				.for_each(|vis| vis.visit_mut(expression, data, chain));
		}

		fn visit_statement_mut(&mut self, statement: &mut Statement, data: &mut T, chain: &Chain) {
			self.statement_visitors_mut
				.iter_mut()
				.for_each(|vis| vis.visit_mut(statement, data, chain));
		}

		fn visit_jsx_element_mut(&mut self, element: &mut JSXElement, data: &mut T, chain: &Chain) {
			self.jsx_element_visitors_mut
				.iter_mut()
				.for_each(|vis| vis.visit_mut(element, data, chain));
		}

		fn visit_variable_mut(
			&mut self,
			variable: &mut MutableVariablePart,
			data: &mut T,
			chain: &Chain,
		) {
			self.variable_visitors_mut
				.iter_mut()
				.for_each(|vis| vis.visit_mut(variable, data, chain));
		}

		fn visit_block_mut(&mut self, block: &mut BlockLikeMut, data: &mut T, chain: &Chain) {
			self.block_visitors_mut.iter_mut().for_each(|vis| vis.visit_mut(block, data, chain));
		}
	}

	// Implementors for functions...
	// impl<T, U> VisitorMut<Expression, T> for U
	// where
	// 	U: Fn(&mut Expression, &mut T),
	// {
	// 	fn visit_mut(&mut self, expression: &mut Expression, data: &mut T, _chain: &Chain) {
	// 		(self)(expression, data)
	// 	}
	// }

	// impl<T, U> VisitorMut<Statement, T> for U
	// where
	// 	U: Fn(&mut Statement, &mut T),
	// {
	// 	fn visit_mut(&mut self, statement: &mut Statement, data: &mut T, _chain: &Chain) {
	// 		(self)(statement, data)
	// 	}
	// }

	// impl<'a, T, U> VisitorMut<MutableVariablePart<'a>, T> for U
	// where
	// 	U: Fn(&mut MutableVariablePart, &mut T),
	// {
	// 	fn visit_mut(&mut self, item: &mut MutableVariablePart, data: &mut T, _chain: &Chain) {
	// 		(self)(item, data)
	// 	}
	// }
}

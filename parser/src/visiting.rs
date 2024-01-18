//! Contains logic that makes viewing and transform parts of AST trivial through the visitor pattern

use source_map::SourceId;

use crate::{
	ArrayDestructuringField, Expression, ObjectDestructuringField, PropertyKey,
	StatementOrDeclaration, WithComment,
};

pub use temporary_annex::Annex;

pub use ast::*;
pub use structures::*;
pub use visitors::*;
pub use visitors_mut::*;

mod ast {
	use temporary_annex::Annex;

	use crate::block::{BlockLike, BlockLikeMut};

	use super::{
		BlockItem, BlockItemMut, Chain, Expression, ImmutableVariableOrProperty,
		MutableVariableOrProperty, VisitorMutReceiver, VisitorReceiver,
	};

	/// Options for behavior when visiting AST.
	/// Customizable behavior is important for analysis
	pub struct VisitOptions {
		/// Visits statements in reverse,
		/// e.g
		/// ```ts
		/// const x = 2;
		/// const y = 3;
		/// ```
		/// If `reverse_statements` is true, `const y = 3` will be visited before `const x = 2`
		pub reverse_statements: bool,
		/// Will not visit anything under nested blocks, functions etc
		pub visit_nested_blocks: bool,
	}

	impl Default for VisitOptions {
		fn default() -> Self {
			Self { reverse_statements: false, visit_nested_blocks: true }
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

	/// Yielded during visiting AST. Might not be 1:1 with AST and include a wrapping type
	pub trait SelfVisitable {}

	/// Yielded during visiting AST. Might not be 1:1 with AST and include a wrapping type
	pub trait SelfVisitableMut {}

	mark_items! {
		impl SelfVisitable for Expression, BlockItem<'_>, BlockLike<'_>, ImmutableVariableOrProperty<'_>
	}

	mark_items! {
		impl SelfVisitableMut for Expression, BlockLikeMut<'_>, BlockItemMut<'_>, MutableVariableOrProperty<'_>
	}

	/// For something to visitable it can visit all nested fields.
	pub trait Visitable {
		fn visit<TData>(
			&self,
			visitors: &mut (impl VisitorReceiver<TData> + ?Sized),
			data: &mut TData,
			options: &VisitOptions,
			chain: &mut Annex<Chain>,
		);

		fn visit_mut<TData>(
			&mut self,
			visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			data: &mut TData,
			options: &VisitOptions,
			chain: &mut Annex<Chain>,
		);
	}

	// Implementing Visitable to some structures that are commonly used in AST
	impl<T: Visitable> Visitable for Box<T> {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitOptions,
			c: &mut Annex<Chain>,
		) {
			Visitable::visit(&**self, v, d, s, c);
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitOptions,
			c: &mut Annex<Chain>,
		) {
			Visitable::visit_mut(&mut **self, v, d, s, c);
		}
	}

	impl<T: Visitable> Visitable for Vec<T> {
		fn visit<TData>(
			&self,
			v: &mut (impl VisitorReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitOptions,
			c: &mut Annex<Chain>,
		) {
			self.iter().for_each(|item| item.visit(v, d, s, c));
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitOptions,
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
			s: &VisitOptions,
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
			s: &VisitOptions,
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
			s: &VisitOptions,
			c: &mut Annex<Chain>,
		) {
			self.0.visit(v, d, s, c);
			self.1.visit(v, d, s, c);
		}

		fn visit_mut<TData>(
			&mut self,
			v: &mut (impl VisitorMutReceiver<TData> + ?Sized),
			d: &mut TData,
			s: &VisitOptions,
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
                        _options: &VisitOptions,
                        _chain: &mut Annex<Chain>,
                    ) {}

                    fn visit_mut<TData>(
                        &mut self,
                        _visitors: &mut (impl VisitorMutReceiver<TData> + ?Sized),
                        _data: &mut TData,
                        _options: &VisitOptions,
                        _chain: &mut Annex<Chain>,
                    ) {}
                }
            )*
        }
    }

	// A bunch of blank implementations for data types that do not have
	// any AST nested / aren't important for visiting.
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
		crate::NumberRepresentation,
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
		crate::declarations::ImportExportName,
		crate::declarations::ImportLocation,
		crate::functions::FunctionHeader,
		crate::functions::MethodHeader,
		crate::VariableKeyword
	];
}

/// Data used when visiting AST
mod structures {
	use crate::{
		property_key::{AlwaysPublic, PublicOrPrivate},
		Statement, VariableFieldInSourceCode, VariableIdentifier,
	};

	use super::{
		ArrayDestructuringField, ObjectDestructuringField, PropertyKey, SourceId,
		StatementOrDeclaration, WithComment,
	};
	use source_map::Span;
	use temporary_annex::{Annex, Annexable};

	#[derive(Debug, Clone)]
	pub enum ChainVariable {
		Module(SourceId),
		Function(Span),
		Block(Span),
	}

	/// Contains [`ChainVariable`]s which signal the position in the AST
	#[derive(Debug, Clone)]
	pub struct Chain(Vec<ChainVariable>);

	impl Chain {
		#[must_use]
		pub fn new() -> Self {
			Self(Vec::with_capacity(10))
		}

		#[must_use]
		pub fn new_with_initial(initial: ChainVariable) -> Self {
			let mut buf = Vec::with_capacity(10);
			buf.push(initial);
			Self(buf)
		}

		#[must_use]
		pub fn len(&self) -> usize {
			self.0.len()
		}

		#[must_use]
		pub fn is_empty(&self) -> bool {
			self.0.is_empty()
		}

		pub fn truncate(&mut self, to_size: usize) {
			self.0.truncate(to_size);
		}

		#[must_use]
		pub fn get_module(&self) -> SourceId {
			if let ChainVariable::Module(source) = self.0.first().unwrap() {
				source.to_owned()
			} else {
				source_map::Nullable::NULL
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

	#[derive(Debug)]
	pub enum ImmutableVariableOrProperty<'a> {
		// TODO maybe WithComment on some of these
		VariableFieldName(&'a str, &'a Span),
		// TODO these should maybe only be the spread variables
		ArrayDestructuringMember(&'a ArrayDestructuringField<VariableFieldInSourceCode>),
		ObjectDestructuringMember(
			&'a WithComment<ObjectDestructuringField<VariableFieldInSourceCode>>,
		),
		ClassName(Option<&'a VariableIdentifier>),
		FunctionName(Option<&'a VariableIdentifier>),
		ClassPropertyKey(&'a PropertyKey<PublicOrPrivate>),
		ObjectPropertyKey(&'a PropertyKey<AlwaysPublic>),
	}

	#[derive(Debug)]
	pub enum MutableVariableOrProperty<'a> {
		VariableFieldName(&'a mut String),
		// TODO these should maybe only be the spread variables
		ArrayDestructuringMember(&'a mut ArrayDestructuringField<VariableFieldInSourceCode>),
		ObjectDestructuringMember(
			&'a mut WithComment<ObjectDestructuringField<VariableFieldInSourceCode>>,
		),
		ClassName(Option<&'a mut VariableIdentifier>),
		FunctionName(Option<&'a mut VariableIdentifier>),
		ClassPropertyKey(&'a mut PropertyKey<PublicOrPrivate>),
		ObjectPropertyKey(&'a mut PropertyKey<AlwaysPublic>),
	}

	impl<'a> ImmutableVariableOrProperty<'a> {
		#[must_use]
		pub fn get_variable_name(&self) -> Option<&'a str> {
			match self {
				ImmutableVariableOrProperty::VariableFieldName(name, _) => Some(name),
				ImmutableVariableOrProperty::ArrayDestructuringMember(a) => match a {
					ArrayDestructuringField::Spread(_, VariableIdentifier::Standard(a, _)) => {
						Some(a.as_str())
					}
					_ => None,
				},
				ImmutableVariableOrProperty::ObjectDestructuringMember(o) => {
					match o.get_ast_ref() {
						ObjectDestructuringField::Spread(VariableIdentifier::Standard(a, _), _)
						| ObjectDestructuringField::Name(
							VariableIdentifier::Standard(a, _),
							_,
							_,
						) => Some(a.as_str()),
						_ => None,
					}
				}
				ImmutableVariableOrProperty::FunctionName(name)
				| ImmutableVariableOrProperty::ClassName(name) => {
					if let Some(VariableIdentifier::Standard(name, _)) = name {
						Some(name)
					} else {
						None
					}
				}
				ImmutableVariableOrProperty::ObjectPropertyKey(_property) => {
					// Just want variable names
					None
					// match property.get_ast_ref() {
					// 	PropertyKey::Ident(ident, _, _)
					// 	| PropertyKey::StringLiteral(ident, _, _) => Some(ident.as_str()),
					// 	PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => None,
					// }
				}
				ImmutableVariableOrProperty::ClassPropertyKey(_property) => {
					// Just want variable names
					None
					// match property.get_ast_ref() {
					// 	PropertyKey::Ident(ident, _, _)
					// 	| PropertyKey::StringLiteral(ident, _, _) => Some(ident.as_str()),
					// 	PropertyKey::NumberLiteral(_, _) | PropertyKey::Computed(_, _) => None,
					// }
				}
			}
		}

		#[must_use]
		pub fn get_position(&self) -> &Span {
			use crate::ASTNode;
			match self {
				ImmutableVariableOrProperty::FunctionName(pos)
				| ImmutableVariableOrProperty::ClassName(pos) => match pos {
					Some(p) => p.get_position(),
					None => &source_map::Nullable::NULL,
				},
				ImmutableVariableOrProperty::VariableFieldName(_, pos) => pos,
				ImmutableVariableOrProperty::ArrayDestructuringMember(m) => m.get_position(),
				ImmutableVariableOrProperty::ObjectDestructuringMember(m) => m.get_position(),
				ImmutableVariableOrProperty::ClassPropertyKey(k) => k.get_position(),
				ImmutableVariableOrProperty::ObjectPropertyKey(k) => k.get_position(),
			}
		}
	}

	/// Wrapper type for [`StatementOrDeclaration`]. Needed because [`crate::Statement`] doesn't
	/// come under [`StatementOrDeclaration`] in the case of [`crate::BlockOrSingleStatement`]
	pub enum BlockItem<'a> {
		StatementOrDeclaration(&'a crate::StatementOrDeclaration),
		SingleStatement(&'a crate::Statement),
	}

	impl<'a> From<&'a StatementOrDeclaration> for BlockItem<'a> {
		fn from(item: &'a StatementOrDeclaration) -> Self {
			BlockItem::StatementOrDeclaration(item)
		}
	}

	impl<'a> From<&'a Statement> for BlockItem<'a> {
		fn from(item: &'a Statement) -> Self {
			BlockItem::SingleStatement(item)
		}
	}

	/// Wrapper type for [`StatementOrDeclaration`]. Needed because [`crate::Statement`] doesn't
	/// come under [`StatementOrDeclaration`] in the case of [`crate::BlockOrSingleStatement`]
	pub enum BlockItemMut<'a> {
		StatementOrDeclaration(&'a mut crate::StatementOrDeclaration),
		SingleStatement(&'a mut crate::Statement),
	}

	impl<'a> From<&'a mut StatementOrDeclaration> for BlockItemMut<'a> {
		fn from(item: &'a mut StatementOrDeclaration) -> Self {
			BlockItemMut::StatementOrDeclaration(item)
		}
	}

	impl<'a> From<&'a mut Statement> for BlockItemMut<'a> {
		fn from(item: &'a mut Statement) -> Self {
			BlockItemMut::SingleStatement(item)
		}
	}
}

mod visitors {
	use super::{BlockItem, Chain, Expression, ImmutableVariableOrProperty, SelfVisitable};
	use crate::{block::BlockLike, TSXKeyword};
	use source_map::Span;

	/// A visitor over something which is hooked/is [`SelfVisitable`] with some generic `Data`
	pub trait Visitor<Item: SelfVisitable, Data> {
		fn visit(&mut self, item: &Item, data: &mut Data, chain: &Chain);
	}

	/// Something which has a bunch of callbacks for AST
	#[allow(unused_variables)]
	pub trait VisitorReceiver<T> {
		fn visit_expression(&mut self, expression: &Expression, data: &mut T, chain: &Chain) {}

		fn visit_statement(&mut self, statement: BlockItem, data: &mut T, chain: &Chain) {}

		fn visit_variable(
			&mut self,
			variable: &ImmutableVariableOrProperty,
			data: &mut T,
			chain: &Chain,
		) {
		}

		fn visit_block(&mut self, block: &BlockLike, data: &mut T, chain: &Chain) {}

		fn visit_keyword(&mut self, keyword: &(TSXKeyword, &Span), data: &mut T, chain: &Chain) {}
	}

	impl<T> VisitorReceiver<T> for Visitors<T> {
		fn visit_expression(&mut self, expression: &Expression, data: &mut T, chain: &Chain) {
			self.expression_visitors.iter_mut().for_each(|vis| vis.visit(expression, data, chain));
		}

		fn visit_statement(&mut self, statement: BlockItem, data: &mut T, chain: &Chain) {
			self.statement_visitors.iter_mut().for_each(|vis| vis.visit(&statement, data, chain));
		}

		fn visit_variable(
			&mut self,
			variable: &ImmutableVariableOrProperty,
			data: &mut T,
			chain: &Chain,
		) {
			self.variable_visitors.iter_mut().for_each(|vis| vis.visit(variable, data, chain));
		}

		fn visit_block(&mut self, block: &BlockLike, data: &mut T, chain: &Chain) {
			self.block_visitors.iter_mut().for_each(|vis| vis.visit(block, data, chain));
		}

		fn visit_keyword(&mut self, _keyword: &(TSXKeyword, &Span), _data: &mut T, _chain: &Chain) {
		}
	}

	type ExpressionVisitor<T> = Box<dyn Visitor<Expression, T>>;
	type StatementVisitor<T> = Box<dyn for<'a> Visitor<BlockItem<'a>, T>>;
	type VariableVisitor<T> = Box<dyn for<'a> Visitor<ImmutableVariableOrProperty<'a>, T>>;
	type BlockVisitor<T> = Box<dyn for<'a> Visitor<BlockLike<'a>, T>>;

	/// A utility type which implements [`VisitorReceiver`]. Use for running a bunch of different **immutable**
	/// visitors over a **immutable** AST. Used for simple analysis
	#[derive(Default)]
	pub struct Visitors<T> {
		pub expression_visitors: Vec<ExpressionVisitor<T>>,
		pub statement_visitors: Vec<StatementVisitor<T>>,
		pub variable_visitors: Vec<VariableVisitor<T>>,
		pub block_visitors: Vec<BlockVisitor<T>>,
	}

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

	use super::{BlockItemMut, Chain, Expression, MutableVariableOrProperty, SelfVisitableMut};

	/// A visitor over something which is hooked/is [`SelfVisitableMut`] with some Data
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

		fn visit_statement_mut(&mut self, statement: BlockItemMut, data: &mut T, chain: &Chain) {}

		fn visit_variable_mut(
			&mut self,
			variable: &mut MutableVariableOrProperty,
			data: &mut T,
			chain: &Chain,
		) {
		}

		fn visit_block_mut(&mut self, block: &mut BlockLikeMut, data: &mut T, chain: &Chain) {}
	}

	type StatementVisitor<T> = Box<dyn for<'a> VisitorMut<BlockItemMut<'a>, T>>;
	type VariableVisitor<T> = Box<dyn for<'a> VisitorMut<MutableVariableOrProperty<'a>, T>>;
	type BlockVisitor<T> = Box<dyn for<'a> VisitorMut<BlockLikeMut<'a>, T>>;

	/// A utility type which implements [`VisitorMutReceiver`]. Use for running a bunch of different **mutable**
	/// visitors over a **mutable** AST. Therefore can remove, add or change AST
	pub struct VisitorsMut<T> {
		pub expression_visitors_mut: Vec<Box<dyn VisitorMut<Expression, T>>>,
		pub statement_visitors_mut: Vec<StatementVisitor<T>>,
		pub variable_visitors_mut: Vec<VariableVisitor<T>>,
		pub block_visitors_mut: Vec<BlockVisitor<T>>,
	}

	impl<T> Default for VisitorsMut<T> {
		fn default() -> Self {
			Self {
				expression_visitors_mut: Default::default(),
				statement_visitors_mut: Default::default(),
				variable_visitors_mut: Default::default(),
				block_visitors_mut: Default::default(),
			}
		}
	}

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

		fn visit_statement_mut(&mut self, mut item: BlockItemMut, data: &mut T, chain: &Chain) {
			self.statement_visitors_mut
				.iter_mut()
				.for_each(|vis| vis.visit_mut(&mut item, data, chain));
		}

		fn visit_variable_mut(
			&mut self,
			variable: &mut MutableVariableOrProperty,
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

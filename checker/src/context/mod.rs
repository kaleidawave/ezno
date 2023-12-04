//! Environments provide maps between string keys and types and variables
//!
//! They also handle scoping, e.g. what is accessible where and branching

pub mod environment;
mod root;
// TODO better name
mod bases;
pub mod calling;
pub mod facts;

pub(crate) use calling::CallCheckingBehavior;
pub use root::RootContext;

pub(crate) use bases::Boundary;

use source_map::{Span, SpanWithSource};

use crate::{
	behavior::{
		self,
		functions::{ClosureChain, ClosureId, FunctionBehavior, FunctionRegisterBehavior},
		modules::Exported,
		operations::MathematicalAndBitwise,
		variables::{VariableMutability, VariableOrImport},
	},
	diagnostics::{
		CannotRedeclareVariable, TypeCheckError, TypeCheckWarning, TypeStringRepresentation,
	},
	events::{Event, RootReference},
	subtyping::{type_is_subtype, BasicEquality},
	types::{
		self, create_this_before_function_synthesis,
		poly_types::{generic_type_arguments::StructureGenericArguments, FunctionTypeArguments},
		properties::{PropertyKey, PropertyValue},
		Constructor, FunctionType, PolyNature, Type, TypeId, TypeStore,
	},
	ASTImplementation, CheckingData, Constant, FunctionId, ReadFromFS, VariableId,
};

use self::{
	environment::FunctionScope,
	facts::{Facts, Publicity},
};
pub use environment::Scope;
pub(crate) use environment::Syntax;
use map_vec::Map;
use std::{
	borrow::Cow,
	collections::{
		hash_map::{self, Entry},
		HashMap, HashSet,
	},
	hash::Hash,
	iter::{self},
	mem,
	sync::atomic::AtomicU16,
};

pub type Environment<'a> = Context<environment::Syntax<'a>>;

static ENVIRONMENT_ID_COUNTER: AtomicU16 = AtomicU16::new(1);

#[derive(PartialEq, Eq, Clone, Copy, derive_debug_extras::DebugExtras, Hash)]
pub struct ContextId(u16);

impl ContextId {
	pub fn new() -> Self {
		Self(ENVIRONMENT_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
	}

	/// **Use with care**
	pub const NULL: Self = Self(0);

	pub const ROOT: Self = Self(1);
}

impl Default for ContextId {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Debug, Clone)]
pub enum GeneralContext<'a> {
	Syntax(&'a Environment<'a>),
	Root(&'a RootContext),
}

/// Used for doing things with a Context that is either [Root] or [Environment]
macro_rules! get_on_ctx {
	(&$env:ident$(.$field:ident)*) => {
		match $env {
			crate::context::GeneralContext::Syntax(env) => &env$(.$field)*,
			crate::context::GeneralContext::Root(env) => &env$(.$field)*
		}
    };
	($env:ident$(.$field:ident)*) => {
		match $env {
			crate::context::GeneralContext::Syntax(env) => env$(.$field)*,
			crate::context::GeneralContext::Root(env) => env$(.$field)*
		}
    };

	($env:ident$(.$field:ident)*($($arg:expr),*)) => {
		match $env {
			crate::context::GeneralContext::Syntax(env) => env$(.$field)*($($arg),*),
			crate::context::GeneralContext::Root(env) => env$(.$field)*($($arg),*)
		}
    };

	(either $env:ident$(.$field:ident)*($($arg:expr),*)) => {
		match $env {
			crate::context::GeneralContext::Syntax(env) => either::Either::Left(env$(.$field)*($($arg),*)),
			crate::context::GeneralContext::Root(env) => either::Either::Right(env$(.$field)*($($arg),*))
		}
    };
}

pub(crate) use get_on_ctx;

pub type ClosedOverReferencesInScope = HashSet<RootReference>;

pub type MutableRewrites = Vec<(VariableId, VariableId)>;

pub trait ContextType: Sized {
	fn as_general_context(et: &Context<Self>) -> GeneralContext<'_>;

	fn get_parent(&self) -> Option<&GeneralContext<'_>>;

	/// Variables **above** this scope may change *between runs*
	fn is_dynamic_boundary(&self) -> bool;

	fn get_closed_over_references(&mut self) -> Option<&mut ClosedOverReferencesInScope>;

	fn get_exports(&mut self) -> Option<&mut Exported>;
}

// TODO enum_from
impl<'a> From<&'a RootContext> for GeneralContext<'a> {
	fn from(root: &'a RootContext) -> Self {
		GeneralContext::Root(root)
	}
}

impl<'a> From<&'a Environment<'a>> for GeneralContext<'a> {
	fn from(env: &'a Environment<'a>) -> Self {
		GeneralContext::Syntax(env)
	}
}

pub struct Names {
	pub(crate) variables: HashMap<String, VariableOrImport>,
	pub(crate) named_types: HashMap<String, TypeId>,

	/// For debugging only
	pub(crate) variable_names: HashMap<VariableId, String>,
}

#[derive(Debug)]
pub struct Context<T: ContextType> {
	// pub(crate) context_id: ContextId,
	pub context_id: ContextId,
	pub(crate) context_type: T,

	pub(crate) variables: HashMap<String, VariableOrImport>,
	pub(crate) named_types: HashMap<String, TypeId>,

	/// For debugging only
	pub(crate) variable_names: HashMap<VariableId, String>,

	/// TODO not sure if needed
	pub(crate) deferred_function_constraints: HashMap<FunctionId, (FunctionType, SpanWithSource)>,
	pub(crate) bases: bases::Bases,

	/// Object type (LHS), must always be RHS
	pub(crate) object_constraints: HashMap<TypeId, Vec<TypeId>>,

	/// TODO replace with facts.value_of_this
	pub(crate) can_reference_this: CanReferenceThis,

	// pub (crate) facts: Facts,
	pub facts: Facts,
}

#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub(super) enum CanReferenceThis {
	NotYetSuperToBeCalled,
	// in constructors, with or without `super`
	ConstructorCalled,
	// Top level scope,
	Yeah,
}

#[derive(Clone)]
pub enum VariableRegisterBehavior {
	Register {
		mutability: VariableMutability,
	},
	Declare {
		/// *wrapped in open poly type*
		base: TypeId,
		context: Option<String>,
	},
	FunctionParameter {
		/// TODO what happens to it
		annotation: Option<TypeId>,
	},
	// TODO document behavior
	CatchVariable {
		ty: TypeId,
	},
	ConstantImport {
		value: TypeId,
	},
}

impl<T: ContextType> Context<T> {
	/// This exists on context because bases are localised
	// TODO with_rule
	pub fn attempt_to_modify_base(
		&mut self,
		on: TypeId,
		boundary: Boundary,
		new_constraint: TypeId,
	) {
		crate::utils::notify!("Modifying #{} to have new base #{}", on.0, new_constraint.0);

		self.bases.mutable_bases.insert(on, (boundary, new_constraint));
		// self.bases.insert(on, new_constraint);

		// let env = self.context_type.get_parent();
		// let starts_on = env.map(|env| get_on_ctx!(env.get_type_counter())).unwrap_or_default();
		// if let Some(val) = (on.0 as usize).checked_sub(starts_on) {
		// 	match ty {
		// 		Type::AliasTo { to, .. } => {
		// 			*to = new_constraint;
		// 		}
		// 		Type::RootPolyType { aliases, nature } => {
		// 			match nature {
		// 				PolyNature::FreeVariable { reference } => {
		// 					self.context_type
		// 						.get_closed_over_references_mut()
		// 						.insert(*reference, new_constraint);

		// 					*aliases = new_constraint;
		// 				}
		// 				PolyNature::Generic(_) | PolyNature::Parameter => {
		// 					crate::utils::notify!("Updated constraint on generic parameter, fine for structure generics");
		// 					*aliases = new_constraint;
		// 				}
		// 				PolyNature::UnsynthesisedFunction(_) => todo!(),
		// 			}
		// 		}
		// 		Type::Constructor(constructor) => match constructor {
		// 			Constructor::BinaryOperator { operator, lhs, rhs } => todo!(),
		// 			Constructor::UnaryOperator { operator, operand } => todo!(),
		// 			Constructor::ConditionalTernary { on, t_res, f_res } => todo!(),
		// 			Constructor::FunctionResult { on, with } => {
		// 				let on = *on;

		// 				let on_constraint = self.get_constraint(on).unwrap().get_type();

		// 				debug_assert_ne!(on_constraint, TypeId::ANY_TYPE);

		// 				todo!("Should be function restriction");
		// 				let function_type = self.get_function(on_constraint).unwrap();

		// 				function_type.return_type = new_constraint;
		// 			}
		// 			Constructor::Property { on, under } => {
		// 				let on = *on;
		// 				let under = *under;

		// 				let on_constraint = self.get_constraint(on).unwrap().get_type();

		// 				debug_assert_ne!(on_constraint, TypeId::ANY_TYPE);

		// 				let existing = self
		// 					.proofs
		// 					.property_constraints
		// 					.insert((on_constraint, under), new_constraint);

		// 				crate::utils::notify!(
		// 					"on: {}, under: {}, on_constraint: {}, new_constraint: {}",
		// 					self.debug_type(on),
		// 					self.debug_type(under),
		// 					self.debug_type(on_constraint),
		// 					self.debug_type(new_constraint)
		// 				);
		// 			}
		// 			Constructor::StructureGenerics { on, with } => unreachable!(),
		// 			Constructor::Prototype(_) => todo!("need to modify prototype info here"),
		// 			Constructor::RelationOperator { lhs, operator, rhs } => todo!(),
		// 			Constructor::LogicalOperator { lhs, operator, rhs } => todo!(),
		// 		},
		// 		Type::And(_, _) | Type::Or(_, _) | Type::NamedRooted { .. } => {
		// 			unreachable!("Modifying {:?}", ty)
		// 		}
		// 	}
		// } else {
		// 	let existing = self.modified_constraints.insert(on, new_constraint);
		// 	assert!(existing.is_none());
		// }
	}

	/// Declares a new variable in the environment and returns the new variable
	///
	/// **THIS IS USED FOR HOISTING, DOES NOT SET THE VALUE**
	/// TODO maybe name: `VariableDeclarator` to include destructuring ...?
	/// TODO hoisted vs declared
	pub fn register_variable<'b>(
		&mut self,
		name: &'b str,
		declared_at: SpanWithSource,
		behavior: VariableRegisterBehavior,
		types: &mut TypeStore,
	) -> Result<TypeId, CannotRedeclareVariable<'b>> {
		let id = VariableId(declared_at.source, declared_at.start);
		self.variable_names.insert(id, name.to_owned());

		let (existing_variable, ty) = match behavior {
			VariableRegisterBehavior::Declare { base, context } => {
				return self.declare_variable(name, declared_at, base, types, context);
			}
			VariableRegisterBehavior::CatchVariable { ty } => {
				// TODO
				let kind = VariableMutability::Constant;
				let variable =
					VariableOrImport::Variable { declared_at, mutability: kind, context: None };
				self.variables.insert(name.to_owned(), variable);
				self.facts.variable_current_value.insert(id, ty);
				(false, ty)
			}
			VariableRegisterBehavior::Register { mutability } => {
				let variable =
					VariableOrImport::Variable { mutability, declared_at, context: None };
				let entry = self.variables.entry(name.to_owned());
				if let Entry::Vacant(empty) = entry {
					empty.insert(variable);
					(false, TypeId::ANY_TYPE)
				} else {
					(true, TypeId::ANY_TYPE)
				}
			}
			VariableRegisterBehavior::FunctionParameter { annotation } => {
				// TODO maybe store separately

				// TODO via a setting
				const FUNCTION_PARAM_MUTABLE: bool = true;
				let mutability = if FUNCTION_PARAM_MUTABLE {
					VariableMutability::Mutable { reassignment_constraint: annotation }
				} else {
					VariableMutability::Constant
				};
				let variable =
					VariableOrImport::Variable { mutability, declared_at, context: None };
				let entry = self.variables.entry(name.to_owned());
				if let Entry::Vacant(empty) = entry {
					let existing_variable = self.variables.insert(name.to_owned(), variable);
					let parameter_ty = if let Some(annotation) = annotation {
						if let Type::RootPolyType(_) = types.get_type_by_id(annotation) {
							// TODO this can only be used once, two parameters cannot have same type as the can
							// also not be equal thing to be inline with ts...
							annotation
						} else {
							types.register_type(Type::RootPolyType(
								crate::types::PolyNature::Parameter { fixed_to: annotation },
							))
						}
					} else {
						let fixed_to = TypeId::ANY_TYPE;
						types.register_type(Type::RootPolyType(
							crate::types::PolyNature::Parameter { fixed_to },
						))
					};

					self.facts.variable_current_value.insert(id, parameter_ty);
					(false, parameter_ty)
				} else {
					(true, TypeId::ERROR_TYPE)
				}
			}
			VariableRegisterBehavior::ConstantImport { value } => {
				let variable = VariableOrImport::Variable {
					mutability: VariableMutability::Constant,
					declared_at,
					context: None,
				};
				let entry = self.variables.entry(name.to_owned());
				self.facts.variable_current_value.insert(id, value);
				if let Entry::Vacant(empty) = entry {
					empty.insert(variable);
					(false, TypeId::ANY_TYPE)
				} else {
					(true, TypeId::ANY_TYPE)
				}
			}
		};

		if existing_variable {
			Err(CannotRedeclareVariable { name })
		} else {
			Ok(ty)
		}
	}

	pub fn register_variable_handle_error<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		name: &str,
		declared_at: SpanWithSource,
		behavior: VariableRegisterBehavior,
		checking_data: &mut CheckingData<U, A>,
	) -> TypeId {
		if let Ok(ty) =
			self.register_variable(name, declared_at, behavior, &mut checking_data.types)
		{
			ty
		} else {
			checking_data.diagnostics_container.add_error(
				TypeCheckError::CannotRedeclareVariable {
					name: name.to_owned(),
					position: declared_at,
				},
			);
			TypeId::ERROR_TYPE
		}
	}

	pub(crate) fn debug(&self) -> String {
		use std::fmt::Write;
		const INDENT: &str = "\t";

		let collect = self.parents_iter().collect::<Vec<_>>();
		// crate::utils::notify!("Debugging found {} contexts", collect.len());
		// crate::utils::notify!("{:?}", self.facts.variable_current_value);

		let enumerate = collect.into_iter().rev().enumerate();
		let mut buf = String::new();
		for (indent, ctx) in enumerate {
			let indent = INDENT.repeat(indent + 1);

			let types = get_on_ctx!(ctx.named_types.len());
			let variables = get_on_ctx!(ctx.variables.len());
			let ty = if let GeneralContext::Syntax(syn) = ctx {
				match &syn.context_type.scope {
					Scope::Function { .. } => "function",
					Scope::InterfaceEnvironment { .. } => "interface",
					Scope::FunctionAnnotation {} => "function reference",
					Scope::Conditional { .. } => "conditional",
					Scope::Looping { .. } => "looping",
					Scope::TryBlock { .. } => "try",
					Scope::Block {} => "block",
					Scope::Module { .. } => "module",
					Scope::TypeAlias => "type alias",
					Scope::DefinitionModule { .. } => "definition module",
					Scope::PassThrough { .. } => "pass through",
					Scope::StaticBlock { .. } => "static block",
				}
			} else {
				"root"
			};

			writeln!(buf, "{}Context#{}({}) ", indent, get_on_ctx!(ctx.context_id.clone()).0, ty)
				.unwrap();
			writeln!(
				buf,
				"{}{} types, {} variables, this={:?}",
				indent,
				types,
				variables,
				get_on_ctx!(&ctx.can_reference_this)
			)
			.unwrap();
			// writeln!(
			// 	buf,
			// 	"{}Variables {:?}",
			// 	indent,
			// 	get_on_ctx!(&ctx.facts.variable_current_value)
			// )
			// .unwrap();
			if let GeneralContext::Syntax(syn) = ctx {
				if !syn.facts.events.is_empty() {
					writeln!(buf, "{indent}> Events:").unwrap();
					for event in &syn.facts.events {
						writeln!(buf, "{indent}   {event:?}").unwrap();
					}
				}
			}
		}
		buf
	}

	/// Finds the constraint of poly types
	pub(crate) fn get_poly_base(&self, on: TypeId, types: &TypeStore) -> Option<TypeId> {
		match types.get_type_by_id(on) {
			Type::RootPolyType(nature) => {
				fn does_type_have_mutable_constraint<T: ContextType>(
					context: &Context<T>,
					on: TypeId,
				) -> bool {
					context.parents_iter().any(|env| {
						if let GeneralContext::Syntax(syn) = env {
							syn.bases.does_type_have_mutable_base(on)
						} else {
							false
						}
					})
				}

				let based_on = match nature {
					PolyNature::Parameter { fixed_to } => fixed_to,
					PolyNature::Generic { name, eager_fixed } => eager_fixed,
					PolyNature::Open(ty) => ty,
					PolyNature::FreeVariable { reference, based_on } => based_on,
					PolyNature::RecursiveFunction(_, return_ty) => return_ty,
				};

				// TODO not sure

				Some(*based_on)

				// // TODO into function
				// match nature.get_poly_pointer() {
				// 	PolyPointer::Fixed(to) => Some(PolyBase::Fixed {
				// 		to,
				// 		is_open_poly: matches!(nature, PolyNature::Open(..)),
				// 	}),
				// 	PolyPointer::Inferred(boundary) => {
				// 		let to = self
				// 			.parents_iter()
				// 			.find_map(|ctx| get_on_ctx!(ctx.bases.get_local_type_base(on)))
				// 			// TODO temp
				// 			.unwrap_or_else(|| {
				// 				crate::utils::notify!("No type base on inferred poly type");
				// 				TypeId::ANY_TYPE
				// 			});

				// 		Some(PolyBase::Dynamic { to, boundary })
				// 	}
				// }

				// if let Some(to) = .m {
				// 	Some(PolyBase::Fixed {
				// 		to,
				// 		is_open_poly: matches!(nature, PolyNature::Open(_)),
				// 	})
				// } else {
				// 	Some(PolyBase::Dynamic { to: (), boundary: () })

				// 	// let modified_base =
				// 	// 	self.parents_iter().find_map(|env| get_on_ctx!(env.bases.get(&on)).copied());

				// 	// let aliases = modified_base.unwrap_or(*aliases);

				// 	// Some(if constraint_is_mutable {
				// 	// 	PolyBase::Dynamic(aliases)
				// 	// } else {
				// 	// })
				// }
			}
			Type::Constructor(constructor) => match constructor.clone() {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					if let MathematicalAndBitwise::Add = operator {
						let lhs = self.get_thingy(lhs, types);
						let rhs = self.get_thingy(rhs, types);
						// TODO these need to be generated
						if let (TypeId::NUMBER_TYPE, TypeId::NUMBER_TYPE) = (lhs, rhs) {
							Some(TypeId::NUMBER_TYPE)
						} else if let (TypeId::STRING_TYPE, _) | (_, TypeId::STRING_TYPE) =
							(lhs, rhs)
						{
							Some(TypeId::STRING_TYPE)
						} else {
							// TODO new conditional
							todo!("needs conditional {:?} {:?}", lhs, rhs)
						}
					} else {
						Some(TypeId::NUMBER_TYPE)
					}
				}
				Constructor::UnaryOperator { operand, operator } => {
					todo!()
					// if *constraint == TypeId::ANY_TYPE && mutable_context {
					// 	let (operand, operator) = (operand.clone(), operator.clone());
					// 	let constraint = to(self, data);
					// 	self.modify_type(
					// 		on,
					// 		Some(Type::Constructor(Constructor::UnaryOperator {
					// 			operator,
					// 			operand,
					// 								// 		})),
					// 	);
					// 	Some(constraint)
					// } else {
					// 	Some(*constraint)
					// }
				}
				Constructor::FunctionResult { on, with, result } => {
					Some(result)
					// TODO temp
					// if let PolyPointer::Fixed(result) = result {
					// 	Some(PolyBase::Fixed { to: result, is_open_poly: true })
					// } else {
					// 	let on_base_function = self.get_poly_base(on, types);
					// 	if let Some(base) = on_base_function {
					// 		let (boundary, is_open_poly, ty) = base.unravel();
					// 		if let Type::Function(func, _) = types.get_type_by_id(ty) {
					// 			Some(func.return_type)
					// 		} else {
					// 			todo!()
					// 		}
					// 	} else {
					// 		// TODO record ahead of time, rather than recalculating here
					// 		let is_open_poly = with
					// 			.iter()
					// 			.filter_map(|arg| {
					// 				self.get_poly_base(arg.into_type().unwrap(), types)
					// 			})
					// 			.all(|base| base.is_open_poly());

					// 		let ty = types.get_type_by_id(on);
					// 		if let Type::Function(func, _) = ty {
					// 			// TODO
					// 			Some(func.return_type)
					// 		} else {
					// 			let on = crate::types::printing::print_type(
					// 				on,
					// 				types,
					// 				&self.into_general_context(),
					// 				true,
					// 			);
					// 			unreachable!("Getting function on {}", on);
					// 		}
					// 	}
					// }
				}
				Constructor::Property { on, under, result } => {
					Some(result)

					// `on` or `under` will be poly, but one of them may be a non-poly
					// type and so it can be expected to be `None` here.
					// TODO needs better primitives for controlling this
					// let on_constraint = self.get_poly_base(on, types).unwrap_or(on);
					// let property = match under {
					// 	PropertyKey::Type(ty) => {
					// 		PropertyKey::Type(self.get_poly_base(ty, types).unwrap_or(ty))
					// 	}
					// 	prop => prop,
					// };

					// Bad
					// let is_open_poly =
					// 	on_constraint.as_ref().map(PolyBase::is_open_poly).unwrap_or(true)
					// 		&& property_constraint
					// 			.as_ref()
					// 			.map(PolyBase::is_open_poly)
					// 			.unwrap_or(true);

					// let on_base = on_constraint.unwrap_or(on);
					// let property_base = property_constraint.unwrap_or(under);

					// TODO abstract to function
					// let (on_boundary, _, on_constraint) = on_base.unravel();
					// let (property_fixed, _, property_constraint) = property_base.unravel();

					// TODO temp
					// let result = result self
					// 	.get_property_unbound(on_constraint, PublicityKind::Public, property, types)
					// 	.map(|property| match property {
					// 		Logical::Pure(PropertyValue::Value(v)) => v,
					// 		// TODO not sure?
					// 		Logical::Pure(PropertyValue::Getter(g)) => g.return_type,
					// 		result => todo!("{:?}", result),
					// 	})
					// 	.expect("Inference failed");
				}
				Constructor::ConditionalResult { result_union, .. } => {
					// TODO dynamic and open poly
					Some(result_union)
				}
				Constructor::TypeOperator(_) | Constructor::CanonicalRelationOperator { .. } => {
					// TODO open poly
					Some(TypeId::BOOLEAN_TYPE)
				}
				Constructor::TypeRelationOperator(op) => match op {
					crate::types::TypeRelationOperator::Extends { .. } => {
						Some(TypeId::BOOLEAN_TYPE)
					}
				},
				// TODO sure?
				Constructor::StructureGenerics { .. } => None,
			},
			_ => None,
		}
	}

	fn get_thingy(&self, on: TypeId, types: &TypeStore) -> TypeId {
		if let Some(poly_base) = self.get_poly_base(on, types) {
			poly_base
		} else if let Type::Constant(cst) = types.get_type_by_id(on) {
			cst.get_backing_type_id()
		} else {
			on
		}
	}

	/// Only on current environment, doesn't walk
	fn get_this_constraint(&self) -> Option<TypeId> {
		match self.as_general_context() {
			GeneralContext::Syntax(syn) => match &syn.context_type.scope {
				// Special handling here
				Scope::InterfaceEnvironment { this_constraint }
				| Scope::Function(FunctionScope::Function { this_type: this_constraint, .. }) => {
					Some(*this_constraint)
				}
				Scope::FunctionAnnotation {} => todo!(),
				Scope::Conditional { .. }
				| Scope::Looping { .. }
				| Scope::StaticBlock { .. }
				| Scope::Function(_)
				| Scope::TryBlock { .. }
				| Scope::TypeAlias
				| Scope::Block {}
				| Scope::PassThrough { .. }
				| Scope::DefinitionModule { .. }
				| Scope::Module { .. } => None,
			},
			GeneralContext::Root(root) => None,
		}
	}

	/// Similar to [`Context::get_this_unbound`]
	fn get_variable_unbound(
		&self,
		variable_name: &str,
	) -> Option<(bool, Option<Boundary>, &VariableOrImport)> {
		// crate::utils::notify!(
		// 	"Looking for {:?}, self.variables = {:?}",
		// 	variable_name,
		// 	self.variables.keys().collect::<Vec<_>>()
		// );

		let local_variable = self.variables.get(variable_name);
		if let Some(local) = local_variable {
			let is_root = self.context_type.get_parent().is_none();
			Some((is_root, None, local))
		} else {
			let parent = self.context_type.get_parent()?;
			let (is_root, parent_boundary, var) =
				get_on_ctx!(parent.get_variable_unbound(variable_name))?;

			/* Sometimes the top might not be dynamic (example below) so adding that here.
			```
			let x = 2
			{
				function y() {
					return x
				}
			};
			```
			*/

			let is_dynamic_boundary = self.context_type.is_dynamic_boundary();
			let boundary = if is_dynamic_boundary && parent_boundary.is_none() {
				let boundary = Boundary(get_on_ctx!(parent.context_id));
				Some(boundary)
			} else {
				parent_boundary
			};
			Some((is_root, boundary, var))
		}
	}

	/// Get all properties on a type (for printing and other non one property uses)
	///
	/// - TODO make aware of ands and aliases
	/// - TODO could this be an iterator
	pub fn get_properties_on_type(
		&self,
		base: TypeId,
	) -> Vec<(Publicity, PropertyKey<'static>, TypeId)> {
		let reversed_flattened_properties = self
			.parents_iter()
			.filter_map(|ctx| {
				let id = get_on_ctx!(ctx.context_id);
				let properties = get_on_ctx!(ctx.facts.current_properties.get(&base));
				properties.map(|v| v.iter().rev())
			})
			.flatten();

		let mut deleted_or_existing_properties = HashSet::<PropertyKey>::new();

		let mut properties = Vec::new();
		for (publicity, key, prop) in reversed_flattened_properties {
			if let PropertyValue::Deleted = prop {
				// TODO doesn't cover constants :(
				deleted_or_existing_properties.insert(key.clone());
			} else if deleted_or_existing_properties.insert(key.clone()) {
				properties.push((*publicity, key.to_owned(), prop.as_get_type()));
			}
		}

		properties.reverse();
		properties
	}

	pub(crate) fn get_property_unbound(
		&self,
		on: TypeId,
		publicity: Publicity,
		under: PropertyKey,
		types: &TypeStore,
	) -> Option<Logical<PropertyValue>> {
		fn get_property(
			env: &GeneralContext,
			types: &TypeStore,
			on: TypeId,
			under: (Publicity, &PropertyKey),
		) -> Option<PropertyValue> {
			get_on_ctx!(env.facts.current_properties.get(&on)).and_then(|properties| {
				// TODO rev is important
				properties.iter().rev().find_map(move |(publicity, key, value)| {
					let (want_publicity, want_key) = under;
					if *publicity != want_publicity {
						return None;
					}

					match key {
						PropertyKey::String(string) => {
							if let PropertyKey::String(want) = want_key {
								(string == want).then_some(value.clone())
							} else {
								// TODO
								None
							}
						}
						PropertyKey::Type(key) => {
							match want_key {
								PropertyKey::Type(want) => {
									// TODO backing type...
									if key == want {
										Some(value.clone())
									} else {
										None
									}
								}
								PropertyKey::String(s) => {
									// TODO ...
									if s.parse::<usize>().is_ok() {
										Some(value.clone())
									} else {
										None
									}
								}
							}
						}
					}
				})
			})
		}

		// TODO need actual method for these, aka lowest

		if let Type::SpecialObject(obj) = types.get_type_by_id(on) {
			todo!()
		} else {
			let under = match under {
				PropertyKey::Type(t) => {
					PropertyKey::Type(self.get_poly_base(t, types).unwrap_or(t))
				}
				under @ PropertyKey::String(_) => under,
			};
			types.get_fact_about_type(self, on, &get_property, (publicity, &under))
		}
	}

	/// Note: this also returns base generic types like `Array`
	pub fn get_type_from_name(&self, name: &str) -> Option<TypeId> {
		self.parents_iter().find_map(|env| get_on_ctx!(env.named_types.get(name))).copied()
	}

	pub(crate) fn get_variable_name(&self, id: VariableId) -> &str {
		self.parents_iter().find_map(|env| get_on_ctx!(env.variable_names.get(&id))).unwrap()
	}

	pub fn as_general_context(&self) -> GeneralContext {
		T::as_general_context(self)
	}

	/// TODO doesn't look at aliases using `get_type_fact`!
	pub fn is_frozen(&self, value: TypeId) -> Option<TypeId> {
		self.parents_iter().find_map(|ctx| get_on_ctx!(ctx.facts.frozen.get(&value))).copied()
	}

	// TODO temp declaration
	// TODO should check the TypeId::is_primitive... via aliases + open_poly
	pub(crate) fn is_immutable(&self, value: TypeId) -> bool {
		todo!()
		// let is_frozen = self.is_frozen(value);

		// if is_frozen == Some(TypeId::TRUE) {
		// 	true
		// } else if let Some(
		// 	Constant::Boolean(..)
		// 	| Constant::String(..)
		// 	| Constant::Number(..)
		// ) = self.get_constant_type(value)
		// {
		// 	true
		// } else {
		// 	// notify!("TODO if every type is primitive (or and and checking needed), get constant type should return type Logical");
		// 	false
		// }
	}

	/// Returns a new lexical environment with self as a parent
	/// Use with caution!!1
	pub(crate) fn new_lexical_environment(&self, new_scope: Scope) -> Context<Syntax<'_>> {
		Context {
			context_type: environment::Syntax {
				scope: new_scope,
				parent: T::as_general_context(self),
				free_variables: Default::default(),
				closed_over_references: Default::default(),
				location: None,
			},
			can_reference_this: self.can_reference_this.clone(),
			// TODO maybe based on something in the AST
			context_id: ContextId::new(),
			variables: Default::default(),
			named_types: Default::default(),
			deferred_function_constraints: Default::default(),
			variable_names: Default::default(),
			facts: Default::default(),
			object_constraints: Default::default(),
			bases: Default::default(),
		}
	}

	pub fn new_try_context<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		checking_data: &mut CheckingData<U, A>,
		func: impl for<'a> FnOnce(&'a mut Environment, &'a mut CheckingData<U, A>),
	) -> TypeId {
		let (thrown, ..) = self.new_lexical_environment_fold_into_parent(
			Scope::TryBlock {},
			checking_data,
			|env, cd| {
				func(env, cd);
				let mut thrown = Vec::new();
				let events = mem::take(&mut env.facts.events);
				env.facts.events =
					crate::events::helpers::extract_throw_events(events, &mut thrown);
				thrown
			},
		);

		let mut thrown = thrown.into_iter();

		if let Some(first) = thrown.next() {
			let mut acc = first;
			for next in thrown {
				acc = checking_data.types.new_or_type(acc, next);
			}
			acc
		} else {
			TypeId::NEVER_TYPE
		}
	}

	/// TODO
	/// - Make internal (public methods should substitute for different scopes)
	/// - Make less complex
	pub fn new_lexical_environment_fold_into_parent<
		U: crate::ReadFromFS,
		Res,
		A: crate::ASTImplementation,
	>(
		&mut self,
		scope: Scope,
		checking_data: &mut CheckingData<U, A>,
		cb: impl for<'a> FnOnce(&'a mut Environment, &'a mut CheckingData<U, A>) -> Res,
	) -> (Res, Option<(Vec<Event>, ClosedOverReferencesInScope)>, ContextId) {
		if matches!(scope, Scope::Conditional { .. }) {
			unreachable!("Use Environment::new_conditional_context")
		}

		let mut new_environment = self.new_lexical_environment(scope);
		let res = cb(&mut new_environment, checking_data);
		let context_id = new_environment.context_id;

		let super::Environment {
			context_id,
			variables,
			named_types,
			context_type:
				environment::Syntax {
					scope,
					// Import for parent to be dropped here
					parent: _,
					free_variables: used_parent_references,
					closed_over_references,
					location: _,
				},
			can_reference_this,
			bases,
			variable_names,
			object_constraints,
			deferred_function_constraints,
			mut facts,
		} = new_environment;

		self.bases.merge(bases, self.context_id);

		self.variable_names.extend(variable_names);

		// TODO
		// self.tasks_to_run.extend(tasks_to_run.into_iter());

		// TODO store some information if in LSP mode
		// checking_data.existing_contexts.parent_references.insert(context_id, self.context_id);

		if let Some(current_closed_references) = self.context_type.get_closed_over_references() {
			current_closed_references.extend(closed_over_references);
		}

		// Run any truths through subtyping
		let additional = match scope {
			// TODO these might go
			Scope::FunctionAnnotation {} => None,
			// TODO temp
			Scope::Function(FunctionScope::Constructor { .. }) => {
				Some((facts.events, used_parent_references))
			}

			Scope::Function { .. } => {
				unreachable!("use new_function")
			}
			Scope::Conditional { .. } => {
				unreachable!("use new_conditional")
			}
			Scope::Looping { .. } => {
				// TODO temp
				Some((facts.events, used_parent_references))
			}
			// TODO Scope::Module ??
			Scope::InterfaceEnvironment { .. }
			| Scope::TypeAlias
			| Scope::Block {}
			| Scope::TryBlock {}
			| Scope::PassThrough { .. }
			| Scope::Module { .. }
			| Scope::StaticBlock { .. }
			| Scope::DefinitionModule { .. } => {
				// TODO also lift vars, regardless of scope
				if matches!(scope, Scope::PassThrough { .. }) {
					self.variables.extend(variables);
					self.facts.variable_current_value.extend(facts.variable_current_value);
				} else {
					// TODO for LSP
					// let shell = ExistingContext {
					// 	variables,
					// 	named_types,
					// 	can_reference_this: can_reference_this.clone(),
					// 	scope: scope.clone(),
					// };
					// checking_data.existing_contexts.existing_environments.insert(context_id, shell);
				}

				// 	// TODO temp
				// 	self.context_type
				// 		.get_closed_over_references_mut()
				// 		.extend(closed_over_references.into_iter());

				self.deferred_function_constraints.extend(deferred_function_constraints);

				self.can_reference_this = can_reference_this;

				for (on, mut properties) in facts.current_properties {
					match self.facts.current_properties.entry(on) {
						hash_map::Entry::Occupied(mut occupied) => {
							occupied.get_mut().append(&mut properties);
						}
						hash_map::Entry::Vacant(vacant) => {
							vacant.insert(properties);
						}
					}
				}

				if self.context_type.get_parent().is_some() {
					self.facts.events.append(&mut facts.events);
					None
				} else {
					Some((facts.events, Default::default()))
				}
			}
		};
		(res, additional, context_id)
	}

	/// Returns a iterator of parents. Starting with the current one
	///
	/// TODO should be private
	pub(crate) fn parents_iter(&self) -> impl Iterator<Item = GeneralContext> + '_ {
		iter::successors(Some(self.as_general_context()), |env| {
			if let GeneralContext::Syntax(syn) = env {
				Some(syn.get_parent())
			} else {
				None
			}
		})
	}

	pub fn new_explicit_type_parameter(
		&mut self,
		name: &str,
		constraint_type: Option<TypeId>,
		default_type: Option<TypeId>,
		types: &mut TypeStore,
	) -> crate::types::poly_types::GenericTypeParameter {
		let ty = Type::RootPolyType(PolyNature::Generic {
			name: name.to_owned(),
			// TODO this is fixed!!
			eager_fixed: constraint_type.unwrap_or(TypeId::ANY_TYPE),
		});

		let ty = types.register_type(ty);
		self.named_types.insert(name.to_owned(), ty);

		crate::types::poly_types::GenericTypeParameter {
			name: name.to_owned(),
			id: ty,
			default: default_type,
		}
	}

	pub fn get_type_by_name_handle_errors<U, A: crate::ASTImplementation>(
		&self,
		name: &str,
		pos: SpanWithSource,
		checking_data: &mut CheckingData<U, A>,
	) -> TypeId {
		if let Some(val) = self.get_type_from_name(name) {
			val
		} else {
			checking_data
				.diagnostics_container
				.add_error(TypeCheckError::CouldNotFindType(name, pos));

			TypeId::ERROR_TYPE
		}
	}

	/// TODO extends
	pub fn new_interface<'a, U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		name: &str,
		nominal: bool,
		parameters: Option<&'a [A::TypeParameter<'a>]>,
		extends: Option<&'a [A::TypeAnnotation<'a>]>,
		position: SpanWithSource,
		checking_data: &mut CheckingData<U, A>,
	) -> TypeId {
		let existing = if let Some(id) = self.named_types.get(name) {
			if let Type::Interface { .. } = checking_data.types.get_type_by_id(*id) {
				checking_data
					.diagnostics_container
					.add_warning(TypeCheckWarning::MergingInterfaceInSameContext { position });

				Some(*id)
			} else {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::TypeAlreadyDeclared { name: name.to_owned(), position },
				);
				return TypeId::ERROR_TYPE;
			}
		} else {
			self.parents_iter().find_map(|env| get_on_ctx!(env.named_types.get(name))).and_then(
				|id| {
					matches!(checking_data.types.get_type_by_id(*id), Type::Interface { .. })
						.then_some(*id)
				},
			)
		};

		if let Some(existing) = existing {
			return existing;
		};

		// TODO declare here
		let parameters = parameters.map(|parameters| {
			parameters
				.iter()
				.map(|parameter| {
					let ty = Type::RootPolyType(PolyNature::Generic {
						name: A::type_parameter_name(parameter).to_owned(),
						eager_fixed: TypeId::ANY_TYPE,
					});
					checking_data.types.register_type(ty)
				})
				.collect()
		});

		if let Some(extends) = extends {
			todo!("synthesise, fold into Type::And and create alias type")
		}

		let ty = Type::Interface { nominal, name: name.to_owned(), parameters };
		let interface_ty = checking_data.types.register_type(ty);
		self.named_types.insert(name.to_owned(), interface_ty);
		interface_ty
	}

	pub fn new_alias<'a, U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		name: &str,
		parameters: Option<&'a [A::TypeParameter<'a>]>,
		to: &'a A::TypeAnnotation<'a>,
		position: Span,
		checking_data: &mut CheckingData<U, A>,
	) -> TypeId {
		let mut env = self.new_lexical_environment(Scope::TypeAlias);
		let (parameters, to) = if let Some(parameters) = parameters {
			let parameters = parameters
				.iter()
				.map(|parameter| {
					let name = A::type_parameter_name(parameter).to_owned();
					let ty = Type::RootPolyType(PolyNature::Generic {
						name: name.clone(),
						eager_fixed: TypeId::ANY_TYPE,
					});
					let ty = checking_data.types.register_type(ty);
					// TODO declare type
					env.named_types.insert(name, ty);
					ty
				})
				.collect();

			let to = A::synthesise_type_annotation(to, &mut env, checking_data);
			(Some(parameters), to)
		} else {
			// TODO should just use self
			let to = A::synthesise_type_annotation(to, &mut env, checking_data);
			(None, to)
		};

		// Works as an alias
		let ty = Type::AliasTo { to, name: name.to_owned(), parameters };
		let alias_ty = checking_data.types.register_type(ty);
		let existing_type = self.named_types.insert(name.to_owned(), alias_ty);

		if existing_type.is_some() {
			checking_data.diagnostics_container.add_error(TypeCheckError::TypeAlreadyDeclared {
				name: name.to_owned(),
				position: position.with_source(self.get_source()),
			});
			TypeId::ERROR_TYPE
		} else {
			alias_ty
		}
	}

	pub fn register_initial_variable_declaration_value(
		&mut self,
		id: VariableId,
		value_ty: TypeId,
	) {
		self.facts.variable_current_value.insert(id, value_ty);
	}

	/// TODO remove types
	pub(crate) fn declare_variable<'a>(
		&mut self,
		name: &'a str,
		declared_at: SpanWithSource,
		variable_ty: TypeId,
		types: &mut TypeStore,
		context: Option<String>,
	) -> Result<TypeId, CannotRedeclareVariable<'a>> {
		let id = crate::VariableId(declared_at.source, declared_at.start);

		let kind = VariableMutability::Constant;
		let variable = VariableOrImport::Variable { declared_at, mutability: kind, context };
		let entry = self.variables.entry(name.to_owned());
		if let Entry::Vacant(vacant) = entry {
			vacant.insert(variable);

			// TODO not sure ...
			let ty = if let Type::Function(..) = types.get_type_by_id(variable_ty) {
				variable_ty
			} else {
				types.register_type(Type::RootPolyType(PolyNature::Open(variable_ty)))
			};

			self.facts.variable_current_value.insert(id, ty);
			Ok(ty)
		} else {
			Err(CannotRedeclareVariable { name })
		}
	}

	/// TODO speed up
	pub(crate) fn get_object_constraint(&self, on: TypeId) -> Vec<TypeId> {
		self.parents_iter()
			.flat_map(|env| {
				get_on_ctx!(env.object_constraints.get(&on))
					.iter()
					.copied()
					.flatten()
					.copied()
					.collect::<Vec<_>>()
			})
			.collect()
	}

	pub(crate) fn facts_chain(&self) -> impl Iterator<Item = &'_ Facts> {
		self.parents_iter().map(|env| get_on_ctx!(&env.facts))
	}

	pub(crate) fn get_value_of_this(
		&mut self,
		types: &TypeStore,
		position: &SpanWithSource,
	) -> TypeId {
		self.parents_iter()
			.find_map(|env| {
				if let GeneralContext::Syntax(ctx) = env {
					match ctx.context_type.scope {
						Scope::Function(
							FunctionScope::ArrowFunction { free_this_type, .. }
							| FunctionScope::MethodFunction { free_this_type, .. },
						) => Some(free_this_type),
						Scope::Function(FunctionScope::Constructor {
							this_object_type, ..
						}) => Some(this_object_type),
						Scope::Function(FunctionScope::Function { this_type, .. }) => {
							Some(this_type)
						}
						_ => None,
					}
				} else {
					crate::utils::notify!("TODO get root type");
					Some(TypeId::ERROR_TYPE)
				}
			})
			.unwrap()
	}

	pub(crate) fn get_source(&self) -> source_map::SourceId {
		self.parents_iter()
			.find_map(|ctx| {
				if let GeneralContext::Syntax(Context {
					context_type:
						Syntax {
							scope:
								Scope::Module { source, .. }
								| Scope::DefinitionModule { source }
								| Scope::PassThrough { source },
							..
						},
					..
				}) = ctx
				{
					Some(*source)
				} else {
					None
				}
			})
			.unwrap_or_else(|| {
				panic!("no module {:?}", self.context_id);
			})
	}

	pub(crate) fn get_value_of_constant_import_variable(&self, variable: VariableId) -> TypeId {
		*self
			.parents_iter()
			.find_map(|ctx| get_on_ctx!(ctx.facts.variable_current_value.get(&variable)))
			.unwrap()
	}
}

pub enum AssignmentError {
	/// Non writable, could have position info
	Constant(SpanWithSource),
	VariableNotFound {
		variable: String,
		assignment_position: SpanWithSource,
	},
	/// Covers both assignment and declaration
	DoesNotMeetConstraint {
		variable_type: TypeStringRepresentation,
		variable_site: SpanWithSource,
		value_type: TypeStringRepresentation,
		value_site: SpanWithSource,
	},
	PropertyConstraint {
		property_type: TypeStringRepresentation,
		value_type: TypeStringRepresentation,
		assignment_position: SpanWithSource,
	},
}

/// Completely magic!
#[derive(Debug)]
pub enum Logical<T> {
	Pure(T),
	Or {
		left: Box<Self>,
		right: Box<Self>,
	},
	/// TODO better name, from StructureGenerics
	Implies {
		on: Box<Self>,
		antecedent: StructureGenericArguments,
	},
}

impl<'a, T: Clone> Logical<&'a T> {
	#[must_use]
	pub fn cloned(self) -> Logical<T> {
		match self {
			Logical::Pure(t) => Logical::Pure(t.clone()),
			Logical::Or { .. } => todo!(),
			Logical::Implies { on, antecedent } => {
				Logical::Implies { on: Box::new(on.cloned()), antecedent }
			}
		}
	}
}

// TODO temp
impl Logical<TypeId> {
	#[allow(clippy::wrong_self_convention)]
	pub(crate) fn to_type(self) -> TypeId {
		match self {
			Logical::Pure(ty) => ty,
			Logical::Or { .. } => todo!(),
			Logical::Implies { .. } => todo!(),
		}
	}
}

// TODO temp
impl Logical<PropertyValue> {
	pub(crate) fn prop_to_type(self) -> TypeId {
		match self {
			Logical::Pure(ty) => ty.as_get_type(),
			Logical::Or { .. } => todo!(),
			Logical::Implies { antecedent, on } => {
				let value = on.prop_to_type();
				if let Some((value, a)) = antecedent.type_arguments.get(&value) {
					*value
				} else {
					value
				}
			}
		}
	}
}

#[derive(Debug)]
pub enum SetPropertyError {
	NotWriteable,
	DoesNotMeetConstraint(TypeId, crate::types::subtyping::NonEqualityReason),
}

/// TODO mutable let imports
pub(crate) fn get_value_of_variable<'a>(
	mut facts: impl Iterator<Item = &'a Facts>,
	on: VariableId,
	closures: Option<&impl ClosureChain>,
) -> Option<TypeId> {
	for fact in facts {
		let res = if let Some(closures) = closures {
			closures.get_fact_from_closure(fact, |closure| {
				crate::utils::notify!("Looking in {:?} for {:?}", closure, on);
				fact.closure_current_values.get(&(closure, RootReference::Variable(on))).copied()
			})
		} else {
			None
		};

		let res = res.or_else(|| fact.variable_current_value.get(&on).copied());

		if res.is_some() {
			return res;
		}
	}
	None
}

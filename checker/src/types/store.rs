use std::collections::{HashMap, HashSet};

use crate::{
	behavior::functions::{ClosureId, FunctionBehavior},
	context::{get_on_ctx, Context, ContextType, Logical},
	types::FunctionType,
	types::{PolyNature, Type},
	Environment, FunctionId, GeneralContext, TypeId,
};

use super::{
	get_constraint, properties::PropertyKey, Constructor, StructureGenerics, TypeRelationOperator,
};

/// Holds all the types. Eventually may be split across modules
#[derive(Debug)]
pub struct TypeStore {
	/// Contains all of the types. Indexed by [TypeId]
	types: Vec<Type>,
	pub(crate) functions: HashMap<FunctionId, FunctionType>,

	// TODO
	pub(crate) _dependent_dependencies: HashMap<TypeId, HashSet<TypeId>>,
	// TODO
	pub(crate) _specialisations: HashMap<TypeId, Vec<TypeId>>,

	/// can be used for tree shaking
	pub called_functions: HashSet<FunctionId>,

	/// TODO not best place but is passed through everything so
	pub(crate) closure_counter: u32,
}

impl Default for TypeStore {
	fn default() -> Self {
		// These have to be in the order of TypeId
		let types = vec![
			Type::Interface { name: "error".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "never".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "any".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "boolean".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "number".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "string".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "undefined".to_owned(), parameters: None, nominal: true },
			Type::Interface { name: "null".to_owned(), parameters: None, nominal: true },
			Type::Interface {
				name: "Array".to_owned(),
				parameters: Some(vec![TypeId::T_TYPE]),
				nominal: true,
			},
			// Array T
			Type::RootPolyType(PolyNature::Generic {
				name: "T".to_owned(),
				eager_fixed: TypeId::ANY_TYPE,
			}),
			Type::Interface { name: "object".to_owned(), parameters: None, nominal: false },
			Type::Interface { name: "Function".to_owned(), parameters: None, nominal: false },
			Type::Interface { name: "RegExp".to_owned(), parameters: None, nominal: true },
			Type::Or(TypeId::STRING_TYPE, TypeId::NUMBER_TYPE),
			// true
			Type::Constant(crate::Constant::Boolean(true)),
			// false
			Type::Constant(crate::Constant::Boolean(false)),
			// zero
			Type::Constant(crate::Constant::Number(0.into())),
			// one
			Type::Constant(crate::Constant::Number(1.into())),
			// NaN
			Type::Constant(crate::Constant::NaN),
			// inferred this free variable shortcut
			Type::RootPolyType(PolyNature::FreeVariable {
				reference: crate::events::RootReference::This,
				based_on: TypeId::ANY_TYPE,
			}),
			Type::RootPolyType(PolyNature::Generic {
				name: "new.target".to_owned(),
				// TODO
				eager_fixed: TypeId::ANY_TYPE,
			}),
			// TODO Symbols, needs Constant::Symbol
			Type::AliasTo {
				to: TypeId::ANY_TYPE,
				name: "SymbolToPrimitive".into(),
				parameters: None,
			},
			// `void` type. Does not block sometimes
			Type::AliasTo { to: TypeId::UNDEFINED_TYPE, name: "void".into(), parameters: None },
		];

		// Check that above is correct, TODO eventually a macro
		assert_eq!(types.len(), TypeId::INTERNAL_TYPE_COUNT);

		Self {
			types: types.clone(),
			functions: HashMap::new(),
			_dependent_dependencies: Default::default(),
			_specialisations: Default::default(),
			called_functions: Default::default(),
			closure_counter: 0,
		}
	}
}

impl TypeStore {
	pub fn new_constant_type(&mut self, constant: crate::Constant) -> crate::TypeId {
		// Reuse existing ids rather than creating new types sometimes
		match constant {
			crate::Constant::Number(number) if number == 1f64 => TypeId::ONE,
			crate::Constant::Number(number) if number == 0f64 => TypeId::ZERO,
			crate::Constant::Boolean(value) => {
				if value {
					TypeId::TRUE
				} else {
					TypeId::FALSE
				}
			}
			crate::Constant::Undefined => TypeId::UNDEFINED_TYPE,
			crate::Constant::Null => TypeId::NULL_TYPE,
			crate::Constant::NaN => TypeId::NAN_TYPE,
			_ => {
				let ty = Type::Constant(constant);
				// TODO maybe separate id
				self.register_type(ty)
			}
		}
	}

	pub(crate) fn register_type(&mut self, ty: Type) -> TypeId {
		let id = TypeId(self.types.len().try_into().expect("too many types!"));
		self.types.push(ty);
		id
	}

	#[must_use]
	pub fn get_type_by_id(&self, id: TypeId) -> &Type {
		&self.types[id.0 as usize]
	}

	pub fn new_or_type(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
		let ty = Type::Or(lhs, rhs);
		self.register_type(ty)
	}

	pub fn new_and_type(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
		// TODO distribute or types
		let ty = Type::And(lhs, rhs);
		self.register_type(ty)
	}

	/// TODO temp
	#[must_use]
	pub fn into_vec_temp(self) -> Vec<(TypeId, Type)> {
		self.types.into_iter().enumerate().map(|(idx, ty)| (TypeId(idx as u16), ty)).collect()
	}

	/// From something like: let a: number => string. Rather than a actual function
	pub fn new_function_type_annotation(
		&mut self,
		type_parameters: Option<super::poly_types::GenericTypeParameters>,
		parameters: crate::types::functions::SynthesisedParameters,
		return_type: TypeId,
		declared_at: &source_map::SpanWithSource,
		effects: Vec<crate::events::Event>,
		constant_function: Option<String>,
	) -> TypeId {
		let id = crate::FunctionId(declared_at.source, declared_at.start);
		let function_type = FunctionType {
			type_parameters,
			parameters,
			return_type,
			effects,
			// TODO
			free_variables: Default::default(),
			closed_over_variables: Default::default(),
			// TODO
			behavior: FunctionBehavior::ArrowFunction { is_async: false },
			constant_function,
			id,
		};
		self.functions.insert(id, function_type);
		self.register_type(Type::FunctionReference(id, Default::default()))
	}

	/// TODO this registers 3 new types, is there a smaller way
	pub fn new_conditional_extends_type(
		&mut self,
		check_type: TypeId,
		extends: TypeId,
		true_result: TypeId,
		false_result: TypeId,
	) -> TypeId {
		let on = self.register_type(Type::Constructor(super::Constructor::TypeRelationOperator(
			TypeRelationOperator::Extends { ty: check_type, extends },
		)));
		self.new_conditional_type(on, true_result, false_result)
	}

	pub fn new_conditional_type(
		&mut self,
		condition: TypeId,
		truthy_result: TypeId,
		else_result: TypeId,
	) -> TypeId {
		// TODO raise warning
		if truthy_result == else_result {
			return truthy_result;
		}
		// TODO on is negation then swap operands
		let ty = Type::Constructor(super::Constructor::ConditionalResult {
			condition,
			truthy_result,
			else_result,
			result_union: self.new_or_type(truthy_result, else_result),
		});
		self.register_type(ty)
	}

	pub fn new_anonymous_interface_ty(&mut self) -> TypeId {
		let ty = Type::Object(super::ObjectNature::AnonymousTypeAnnotation);
		self.register_type(ty)
	}

	/// Doesn't do constant compilation
	pub(crate) fn new_logical_negation_type(&mut self, operand: TypeId) -> TypeId {
		let ty = Type::Constructor(Constructor::UnaryOperator {
			operator: crate::behavior::operations::PureUnary::LogicalNot,
			operand,
		});
		self.register_type(ty)
	}

	/// Note it does call it over every context
	/// For:
	/// - Properties
	/// - Equality
	/// - Functions
	pub(crate) fn get_fact_about_type<'a, S: ContextType, TData: Copy, TResult>(
		&self,
		ctx: &'a Context<S>,
		on: TypeId,
		resolver: &impl Fn(&GeneralContext<'a>, &TypeStore, TypeId, TData) -> Option<TResult>,
		data: TData,
	) -> Option<Logical<TResult>> {
		match self.get_type_by_id(on) {
			Type::Function(..) => {
				let on_function = ctx
					.parents_iter()
					.find_map(|env| resolver(&env, self, on, data))
					.map(Logical::Pure);

				// TODO undecided on this
				on_function.or_else(|| {
					self.get_fact_about_type(ctx, TypeId::FUNCTION_TYPE, resolver, data)
				})
			}
			Type::FunctionReference(_, _) => {
				let on_function = ctx
					.parents_iter()
					.find_map(|env| resolver(&env, self, on, data))
					.map(Logical::Pure);

				// TODO undecided on this
				on_function.or_else(|| {
					self.get_fact_about_type(ctx, TypeId::FUNCTION_TYPE, resolver, data)
				})
			}
			Type::AliasTo { to, .. } => {
				let property_on_self = ctx
					.parents_iter()
					.find_map(|env| resolver(&env, self, on, data))
					.map(Logical::Pure);

				property_on_self.or_else(|| self.get_fact_about_type(ctx, *to, resolver, data))
			}

			Type::And(left, right) => self
				.get_fact_about_type(ctx, *left, resolver, data)
				.or_else(|| self.get_fact_about_type(ctx, *right, resolver, data)),

			Type::Or(left, right) => {
				// TODO temp
				let left = self.get_fact_about_type(ctx, *left, resolver, data);
				let right = self.get_fact_about_type(ctx, *right, resolver, data);

				match (left, right) {
					(None, None) => None,
					(Some(value), None) | (None, Some(value)) => Some(value),
					(Some(left), Some(right)) => {
						Some(Logical::Or { left: Box::new(left), right: Box::new(right) })
					}
				}
			}
			Type::RootPolyType(_nature) => {
				// TODO None here
				let aliases = get_constraint(on, self).unwrap();
				// Don't think any properties exist on this poly type
				self.get_fact_about_type(ctx, aliases, resolver, data)
			}
			Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
				on,
				arguments,
			})) => {
				let fact_opt = self.get_fact_about_type(ctx, *on, resolver, data);

				fact_opt.map(|fact| Logical::Implies {
					on: Box::new(fact),
					antecedent: arguments.clone(),
				})
			}
			Type::Constructor(_constructor) => {
				// Don't think any properties exist on this poly type
				// TODO None here
				let constraint = get_constraint(on, self).unwrap();
				// TODO might need to send more information here, rather than forgetting via .get_type
				self.get_fact_about_type(ctx, constraint, resolver, data)
			}
			Type::Object(..) | Type::Interface { .. } => ctx
				.parents_iter()
				.find_map(|env| resolver(&env, self, on, data))
				.map(Logical::Pure)
				.or_else(|| {
					if let Some(prototype) = ctx
						.parents_iter()
						.find_map(|ctx| get_on_ctx!(ctx.facts.prototypes.get(&on)).copied())
					{
						self.get_fact_about_type(ctx, prototype, resolver, data)
					} else {
						None
					}
				}),
			Type::Constant(cst) => ctx
				.parents_iter()
				.find_map(|env| resolver(&env, self, on, data))
				.map(Logical::Pure)
				.or_else(|| {
					self.get_fact_about_type(ctx, cst.get_backing_type_id(), resolver, data)
				}),
			Type::SpecialObject(_) => todo!(),
		}
	}

	pub fn new_closure_id(&mut self) -> ClosureId {
		self.closure_counter += 1;
		ClosureId(self.closure_counter)
	}

	#[must_use]
	pub fn get_function_from_id(&self, id: FunctionId) -> &FunctionType {
		self.functions.get(&id).unwrap()
	}

	pub fn new_function_type(&mut self, function_type: FunctionType) -> TypeId {
		let id = function_type.id;
		self.functions.insert(id, function_type);
		self.register_type(Type::Function(id, Default::default()))
	}

	/// TODO WIP
	#[allow(clippy::similar_names)]
	pub(crate) fn new_property_on_type_annotation(
		&mut self,
		indexee: TypeId,
		indexer: TypeId,
		environment: &Environment,
	) -> TypeId {
		if let Some(base) = get_constraint(indexee, self) {
			let under = PropertyKey::from_type(indexer, self);
			let ty = Type::Constructor(Constructor::Property {
				on: indexee,
				under,
				result: base,
				bind_this: true,
			});
			self.register_type(ty)
		} else if let Some(prop) = environment.get_property_unbound(
			indexee,
			crate::context::facts::Publicity::Public,
			PropertyKey::from_type(indexer, self),
			&self,
		) {
			match prop {
				crate::context::Logical::Pure(ty) => ty.as_get_type(),
				crate::context::Logical::Or { .. } => todo!(),
				crate::context::Logical::Implies { .. } => todo!(),
			}
		} else {
			crate::utils::notify!("Error: no index on type annotation");
			TypeId::ERROR_TYPE
		}
	}

	/// TODO flags
	pub fn new_regex(&mut self, pattern: String) -> TypeId {
		self.register_type(Type::SpecialObject(crate::behavior::objects::SpecialObjects::Regexp(
			pattern,
		)))
	}
}

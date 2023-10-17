use std::collections::{HashMap, HashSet};

use crate::{
	behavior::functions::ClosureId,
	context::{get_on_ctx, Context, ContextType, Logical},
	types::FunctionType,
	types::{PolyNature, Type},
	utils::EnforcedOrExt,
	FunctionId, GeneralContext, TypeId,
};

use super::{Constructor, StructureGenerics, TypeRelationOperator};

/// Holds all the types. Eventually may be split across modules
#[derive(Debug)]
pub struct TypeStore {
	/// Contains all of the types. Indexed by [TypeId]
	types: Vec<Type>,
	pub(crate) functions: HashMap<FunctionId, FunctionType>,

	pub(crate) dependent_dependencies: HashMap<TypeId, HashSet<TypeId>>,

	// TODO merge into type
	// pub(crate) functions_on_type: HashMap<TypeId, FunctionType>,
	// pub(crate) proxies: HashMap<TypeId, Proxy>,
	pub(crate) specializations: HashMap<TypeId, Vec<TypeId>>,

	/// TODO not best place	but is passed through everything so
	pub(crate) closure_counter: u32,
}

impl Default for TypeStore {
	fn default() -> Self {
		// These have to be in the order of TypeId
		let mut types = vec![
			Type::NamedRooted { name: "error".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "never".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "any".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "boolean".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "number".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "string".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "undefined".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted { name: "null".to_owned(), parameters: None, nominal: true },
			Type::NamedRooted {
				name: "Array".to_owned(),
				parameters: Some(vec![TypeId::T_TYPE]),
				nominal: true,
			},
			// Array T
			Type::RootPolyType(PolyNature::Generic {
				name: "T".to_owned(),
				eager_fixed: TypeId::ANY_TYPE,
			}),
			Type::NamedRooted { name: "object".to_owned(), parameters: None, nominal: false },
			Type::NamedRooted { name: "Function".to_owned(), parameters: None, nominal: false },
			Type::NamedRooted { name: "RegExp".to_owned(), parameters: None, nominal: true },
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
			Type::Constant(crate::Constant::String("length".into())),
			// this arg shortcut
			Type::RootPolyType(PolyNature::ParentScope {
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
		];

		// Check that above is correct, TODO eventually a macro
		assert_eq!(types.len(), TypeId::INTERNAL_TYPE_COUNT);

		Self {
			types: types.to_vec(),
			functions: HashMap::new(),
			dependent_dependencies: Default::default(),
			specializations: Default::default(),
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

	pub fn get_type_by_id(&self, id: TypeId) -> &Type {
		&self.types[id.0 as usize]
	}

	pub fn new_any_parameter<S: ContextType>(&mut self, environment: &mut Context<S>) -> TypeId {
		// if let GeneralContext::Syntax(env) = environment.into_general_context() {
		// 	// TODO not sure about this:
		// 	if environment.context_type.is_dynamic_boundary() {
		// 		crate::utils::notify!("TODO is context different in the param synthesis?");

		// 		environment.bases.mutable_bases.insert(id, (inference_boundary, TypeId::ANY_TYPE));
		// 		return id;
		// 	}
		// }
		TypeId::ANY_TYPE
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
	pub fn into_vec_temp(self) -> Vec<(TypeId, Type)> {
		self.types.into_iter().enumerate().map(|(idx, ty)| (TypeId(idx as u16), ty)).collect()
	}

	/// From something like: let a: number => string. Rather than a actual function
	pub fn new_function_type_annotation(
		&mut self,
		type_parameters: Option<super::poly_types::GenericTypeParameters>,
		parameters: crate::types::functions::SynthesisedParameters,
		return_type: TypeId,
		declared_at: source_map::SpanWithSource,
		effects: Vec<crate::events::Event>,
		constant_id: Option<String>,
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
			kind: crate::types::FunctionKind::Arrow,
			constant_id,
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
		resolver: &impl Fn(GeneralContext<'a>, &TypeStore, TypeId, TData) -> Option<TResult>,
		data: TData,
	) -> Option<Logical<TResult>> {
		match self.get_type_by_id(on) {
			Type::Function(..) => {
				ctx.parents_iter().find_map(|env| resolver(env, self, on, data)).map(Logical::Pure)
			}
			Type::AliasTo { to, .. } => {
				let property_on_self = ctx
					.parents_iter()
					.find_map(|env| resolver(env, self, on, data))
					.map(Logical::Pure);

				property_on_self.or_else(|| self.get_fact_about_type(ctx, *to, resolver, data))
			}
			Type::And(left, right) => self
				.get_fact_about_type(ctx, *left, resolver, data)
				.or_else(|| self.get_fact_about_type(ctx, *right, resolver, data)),
			Type::Or(left, right) => {
				// TODO temp
				let left = self.get_fact_about_type(ctx, *left, resolver, data).map(Box::new);
				let right = self.get_fact_about_type(ctx, *right, resolver, data).map(Box::new);
				left.and_enforced(right).map(Logical::Or)
			}
			Type::RootPolyType(_nature) => {
				let aliases = ctx.get_poly_base(on, self).unwrap();
				// Don't think any properties exist on this poly type
				self.get_fact_about_type(ctx, aliases, resolver, data)
			}
			Type::Constructor(constructor) => {
				if let Constructor::StructureGenerics(StructureGenerics { on, arguments }) =
					constructor
				{
					// TODO could drop some of with here
					let fact_opt = self.get_fact_about_type(ctx, *on, resolver, data);
					fact_opt.map(|fact| Logical::Implies {
						on: Box::new(fact),
						antecedent: arguments.clone(),
					})
				} else {
					// Don't think any properties exist on this poly type
					let constraint = ctx.get_poly_base(on, self).unwrap();
					// TODO might need to send more information here, rather than forgetting via .get_type
					self.get_fact_about_type(ctx, constraint, resolver, data)
				}
			}
			Type::Object(..) | Type::NamedRooted { .. } => ctx
				.parents_iter()
				.find_map(|env| resolver(env, self, on, data))
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
				.find_map(|env| resolver(env, self, on, data))
				.map(Logical::Pure)
				.or_else(|| {
					self.get_fact_about_type(ctx, cst.get_backing_type_id(), resolver, data)
				}),
			Type::FunctionReference(_, _) => todo!(),
			Type::Class(_) => todo!(),
		}
	}

	pub fn new_closure_id(&mut self) -> ClosureId {
		self.closure_counter += 1;
		ClosureId(self.closure_counter)
	}

	pub fn get_function_from_id(&self, id: FunctionId) -> &FunctionType {
		self.functions.get(&id).unwrap()
	}
}

use std::collections::{HashMap, HashSet};

use crate::{
	context::{Context, ContextType, InferenceBoundary},
	types::FunctionType,
	types::{PolyNature, Type},
	GeneralEnvironment, TypeId,
};

use super::TypeRelationOperator;

/// Holds all the types. Eventually may be split across modules
#[derive(Debug)]
pub struct TypeStore {
	/// Contains all of the types. Indexed by [TypeId]
	types: Vec<Type>,
	context_implicit_parameters: HashMap<crate::context::ContextId, HashMap<String, TypeId>>,

	pub(crate) dependent_dependencies: HashMap<TypeId, HashSet<TypeId>>,

	// TODO merge into type
	// pub(crate) functions_on_type: HashMap<TypeId, FunctionType>,
	// pub(crate) proxies: HashMap<TypeId, Proxy>,
	pub(crate) specializations: HashMap<TypeId, Vec<TypeId>>,
}

impl Default for TypeStore {
	fn default() -> Self {
		// These have to be in the order of TypeId
		let mut types = vec![
			Type::NamedRooted { name: "error".to_owned(), parameters: None },
			Type::NamedRooted { name: "never".to_owned(), parameters: None },
			Type::NamedRooted { name: "any".to_owned(), parameters: None },
			Type::NamedRooted { name: "boolean".to_owned(), parameters: None },
			Type::NamedRooted { name: "number".to_owned(), parameters: None },
			Type::NamedRooted { name: "string".to_owned(), parameters: None },
			Type::NamedRooted { name: "undefined".to_owned(), parameters: None },
			Type::NamedRooted { name: "null".to_owned(), parameters: None },
			Type::NamedRooted { name: "Array".to_owned(), parameters: Some(vec![TypeId::T_TYPE]) },
			// Array T
			Type::RootPolyType(PolyNature::Generic {
				name: "T".to_owned(),
				eager_fixed: crate::types::PolyPointer::Fixed(TypeId::ANY_TYPE),
			}),
			Type::NamedRooted { name: "object".to_owned(), parameters: None },
			Type::NamedRooted { name: "Function".to_owned(), parameters: None },
			Type::NamedRooted { name: "RegExp".to_owned(), parameters: None },
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
				based_on: crate::types::PolyPointer::Fixed(TypeId::ANY_TYPE),
			}),
			Type::RootPolyType(PolyNature::Generic {
				name: "new.target".to_owned(),
				// TODO
				eager_fixed: crate::types::PolyPointer::Fixed(TypeId::ANY_TYPE),
			}),
			// TODO Symbols, needs Constant::Symbol
			Type::AliasTo {
				to: TypeId::ANY_TYPE,
				name: "SymbolToPrimitive".into(),
				parameters: None,
			},
			Type::NamedRooted { name: "HTMLElementTagNameMap".to_owned(), parameters: None },
			// Internal
			Type::NamedRooted { name: "Operators".to_owned(), parameters: None },
		];

		// Check that above is correct, TODO eventually a macro
		assert_eq!(types.len(), TypeId::INTERNAL_TYPE_COUNT);

		Self {
			types: types.to_vec(),
			dependent_dependencies: Default::default(),
			specializations: Default::default(),
			context_implicit_parameters: Default::default(),
		}
	}
}

impl TypeStore {
	pub fn new_constant_type(&mut self, constant: crate::Constant) -> crate::TypeId {
		// TODO don't recreate same constant
		let ty = Type::Constant(constant);
		// TODO maybe separate id
		self.register_type(ty)
	}

	pub(crate) fn register_type(&mut self, ty: Type) -> TypeId {
		let id = TypeId(self.types.len().try_into().expect("too many types!"));
		self.types.push(ty);
		id
	}

	pub(crate) fn get_type_by_id(&self, id: TypeId) -> &Type {
		&self.types[id.0 as usize]
	}

	pub(crate) fn debug_type(&self, id: TypeId) -> String {
		format!("#{} {:?}", id.0, self.get_type_by_id(id))
	}

	pub fn new_any_parameter<S: ContextType>(&mut self, environment: &mut Context<S>) -> TypeId {
		if let GeneralEnvironment::Syntax(env) = environment.into_general_environment() {
			// TODO not sure about this:
			if environment.context_type.is_dynamic_boundary() {
				crate::utils::notify!("TODO is context different in the param synthesis?");
				let inference_boundary = InferenceBoundary(environment.context_id);
				let id = self.register_type(Type::RootPolyType(PolyNature::Parameter {
					fixed_to: super::PolyPointer::Inferred(inference_boundary),
				}));

				environment.bases.mutable_bases.insert(id, (inference_boundary, TypeId::ANY_TYPE));
				return id;
			}
		}
		TypeId::ANY_TYPE
	}

	pub fn new_or_type(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
		let ty = Type::Or(lhs, rhs);
		self.register_type(ty)
	}

	pub fn new_and_type(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
		let ty = Type::And(lhs, rhs);
		self.register_type(ty)
	}

	/// TODO temp
	pub fn into_vec_temp(self) -> Vec<(TypeId, Type)> {
		self.types.into_iter().enumerate().map(|(idx, ty)| (TypeId(idx as u16), ty)).collect()
	}

	pub fn new_function_type_reference(
		&mut self,
		type_parameters: Option<super::poly_types::GenericTypeParameters>,
		parameters: crate::types::functions::SynthesizedParameters,
		return_type: TypeId,
		span: source_map::Span,
		effects: Vec<crate::events::Event>,
		constant_id: Option<String>,
	) -> TypeId {
		self.register_type(Type::Function(
			FunctionType {
				type_parameters,
				parameters,
				return_type,
				effects,
				// TODO
				closed_over_references: Default::default(),
				// TODO
				kind: crate::types::FunctionKind::Arrow {
					get_set: crate::GetSetGeneratorOrNone::None,
				},
				constant_id,
				id: crate::context::FunctionId(span),
			},
			super::FunctionNature::Source(None),
		))
	}

	/// TODO this registers 3 new types, is there a smaller way
	pub fn new_conditional_extends_type(
		&mut self,
		check_type: TypeId,
		extends: TypeId,
		true_res: TypeId,
		false_res: TypeId,
	) -> TypeId {
		let on = self.register_type(Type::Constructor(super::Constructor::TypeRelationOperator(
			TypeRelationOperator::Extends { ty: check_type, extends },
		)));
		let result_union = self.register_type(Type::Or(true_res, false_res));
		let ty = Type::Constructor(super::Constructor::ConditionalTernary {
			on,
			true_res,
			false_res,
			result_union,
		});
		self.register_type(ty)
	}

	pub fn new_anonymous_interface_ty(&mut self) -> TypeId {
		let ty = Type::Object(super::ObjectNature::AnonymousTypeAnnotation);
		self.register_type(ty)
	}

	/// From something like: let a: number => string. Rather than a actual function
	pub fn new_type_annotation_function_type(&mut self, function_type: FunctionType) -> TypeId {
		self.register_type(Type::Function(function_type, super::FunctionNature::Reference))
	}
}

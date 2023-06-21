pub mod calling;
mod casts;
pub mod functions;
pub mod indexing;
pub mod operations;
pub mod poly_types;
pub mod printing;
pub mod properties;
pub mod store;
pub mod subtyping;
mod terms;

use derive_debug_extras::DebugExtras;

use iterator_endiate::EndiateIteratorExt;
pub(crate) use poly_types::specialization::*;

pub(crate) use casts::*;
pub use store::TypeStore;
pub use terms::Constant;

use crate::{
	context::{get_ctx, GeneralContext, InferenceBoundary},
	events::RootReference,
	structures::{functions::SynthesizedArgument, operators::*},
	GenericTypeParameters,
};

use crate::context::FunctionId;
use std::{
	collections::{HashMap, HashSet},
	fmt::Debug,
};

use self::functions::SynthesizedParameters;

/// References [Type]
///
/// Not to be confused with [parser::TypeId]
///
/// TODO maybe u32 or u64
/// TODO maybe on environment rather than here
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(pub(crate) u16);

// TODO ids as macro as to not do in [crate::RootEnvironment]
impl TypeId {
	/// Not to be confused with [TypeId::NEVER_TYPE]
	pub const ERROR_TYPE: Self = Self(0);

	pub const NEVER_TYPE: Self = Self(1);

	pub const ANY_TYPE: Self = Self(2);

	pub const BOOLEAN_TYPE: Self = Self(3);
	pub const NUMBER_TYPE: Self = Self(4);
	pub const STRING_TYPE: Self = Self(5);

	pub const UNDEFINED_TYPE: Self = Self(6);
	pub const NULL_TYPE: Self = Self(7);

	/// This is the inner version
	pub const ARRAY_TYPE: Self = Self(8);

	/// Used for Array and Promise
	pub const T_TYPE: Self = Self(9);

	pub const OBJECT_TYPE: Self = Self(10);
	pub const FUNCTION_TYPE: Self = Self(11);
	pub const REGEXP_TYPE: Self = Self(12);
	pub const SYMBOL_TYPE: Self = Self(13);

	/// For more direct stuff and the rules
	pub const TRUE: Self = Self(14);
	pub const FALSE: Self = Self(15);
	pub const ZERO: Self = Self(16);
	pub const ONE: Self = Self(17);
	pub const NAN_TYPE: Self = Self(18);
	/// For arrays
	pub const LENGTH_AS_STRING: Self = Self(19);

	/// For this_arg for type constraints only
	pub const THIS_ARG: Self = Self(20);
	/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target
	pub const NEW_TARGET_ARG: Self = Self(21);

	pub const SYMBOL_TO_PRIMITIVE: Self = Self(22);

	// This exists in TS
	pub const HTML_ELEMENT_TAG_NAME_MAP: Self = Self(23);

	/// TODO explain, also might go
	pub const OPERATORS_SPECIAL: Self = Self(24);

	pub(crate) const INTERNAL_TYPE_COUNT: usize = 25;
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Type {
	/// For
	/// - `extends` interface part (for multiple it to is a `Type::And`)
	/// - Can be used for subtypes (aka N aliases number then more types on top)
	/// - **Does not imply .prototype = **
	AliasTo {
		to: TypeId,
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	/// TODO
	And(TypeId, TypeId),
	Or(TypeId, TypeId),
	RootPolyType(PolyNature),
	/// Also a "Specialized constructor type"
	Constructor(Constructor),
	/// For number and other rooted types
	///
	/// Although they all alias Object
	NamedRooted {
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	Constant(crate::Constant),
	/// TODO function type behind Rc
	Function(FunctionType, FunctionNature),
	Object(ObjectNature), // TODO Proxy,
}

/// TODO difference between untyped and typed parameters and what about parameter based for any
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PolyNature {
	Parameter {
		fixed_to: PolyPointer,
	},
	Generic {
		name: String,
		/// This can be `Dynamic` for interface hoisting
		eager_fixed: PolyPointer,
	},
	Open(TypeId),
	/// For functions and for loops where something in the scope can mutate (so not constant)
	/// between runs.
	ParentScope {
		reference: RootReference,
		based_on: PolyPointer,
	},
	RecursiveFunction(FunctionId, PolyPointer),
	// Object
}

#[derive(Copy, Clone, Debug)]
pub enum PolyPointer {
	Fixed(TypeId),
	Inferred(InferenceBoundary),
}

impl PolyNature {
	pub fn get_fixed_constraint(&self) -> Option<TypeId> {
		match self.get_poly_pointer() {
			PolyPointer::Fixed(ty) => Some(ty),
			PolyPointer::Inferred(_) => None,
		}
	}

	pub fn get_poly_pointer(&self) -> PolyPointer {
		match self {
			Self::Open(ty) => PolyPointer::Fixed(*ty),
			Self::Generic { eager_fixed: pp, .. }
			| Self::ParentScope { based_on: pp, .. }
			| Self::Parameter { fixed_to: pp, .. }
			| Self::RecursiveFunction(_, pp) => *pp,
		}
	}

	pub fn has_fixed_constraint(&self) -> bool {
		self.get_fixed_constraint().is_some()
		// matches!(
		// 	self,
		// 	Self::Open(_)
		// 		| Self::Generic { eager_fixed: Some(_), .. }
		// 		| Self::ParentScope { based_on: Some(_), .. }
		// 		| Self::Parameter { fixed_to: Some(_), .. }
		// 		| Self::RecursiveFunction(_, Some(_))
		// )
	}

	pub fn is_open(&self) -> bool {
		matches!(self, Self::Open(_))
	}
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct FunctionType {
	/// TODO not sure about this field and how it tails with Pi Types
	pub type_parameters: Option<GenericTypeParameters>,
	pub parameters: SynthesizedParameters,
	pub return_type: TypeId,
	/// Side effects of the function
	pub effects: Vec<crate::events::Event>,

	/// TODO type alias
	pub closed_over_references: HashMap<RootReference, TypeId>,

	/// Can be called for constant result
	pub constant_id: Option<String>,

	/// TODO somewhat temp
	pub kind: FunctionKind,

	pub id: FunctionId,
}

/// Decides what to do with `new`
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionKind {
	Arrow { get_set: crate::GetSetGeneratorOrNone },
	Function { function_prototype: TypeId },
	ClassConstructor { class_prototype: TypeId, class_constructor: TypeId },
}

/// TODO needs improvement
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionNature {
	BehindPoly {
		/// TODO function id?
		function_id_if_open_poly: Option<FunctionId>,
		this_type: Option<TypeId>,
	},
	/// Last is 'this' type,
	Source(Option<TypeId>),
	Constructor,
	Reference,
}

#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum ObjectNature {
	/// Actual allocated object
	RealDeal,
	/// From `x: { a: number }` etc
	AnonymousTypeAnnotation,
	/// Important that this is different as properties can be mutated
	ModifiableConstraint,
}

/// Eventually these
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum GetterSetter {
	Getter,
	Setter,
}

impl Type {
	pub(crate) fn get_parameters(&self) -> Option<Vec<TypeId>> {
		if let Type::NamedRooted { parameters, .. } | Type::AliasTo { parameters, .. } = self {
			parameters.clone()
		} else {
			None
		}
	}

	/// TODO return is poly
	pub(crate) fn is_dependent(&self) -> bool {
		match self {
			Type::Constructor(_) | Type::RootPolyType(_) => true,
			Type::And(_, _) | Type::Or(_, _) | Type::AliasTo { .. } | Type::NamedRooted { .. } => {
				// TODO not sure
				false
			}
			Type::Constant(_) | Type::Function(_, _) | Type::Object(_) => false,
		}
	}
}

/// TODO work in progress types
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Constructor {
	BinaryOperator {
		lhs: TypeId,
		operator: BinaryOperator,
		rhs: TypeId,
	},
	RelationOperator {
		lhs: TypeId,
		operator: RelationOperator,
		rhs: TypeId,
	},
	LogicalOperator {
		lhs: TypeId,
		operator: LogicalOperator,
		rhs: TypeId,
	},
	UnaryOperator {
		operator: UnaryOperator,
		operand: TypeId,
	},
	TypeOperator(TypeOperator),
	TypeRelationOperator(TypeRelationOperator),
	/// TODO constraint is res
	ConditionalTernary {
		/// TODO this can only be poly types
		on: TypeId,
		true_res: TypeId,
		false_res: TypeId,
		// Currently necessary for getting the base == `true_res | false_res`
		result_union: TypeId,
	},
	///
	FunctionResult {
		on: TypeId,
		// TODO I don't think this is necessary, maybe for debugging. In such case should be an Rc to share with events
		with: Box<[SynthesizedArgument]>,
		result: PolyPointer,
	},
	Property {
		on: TypeId,
		under: TypeId,
	},
	/// Should really be base, but we will ignore for now
	StructureGenerics {
		on: TypeId,
		with: map_vec::Map<TypeId, TypeId>,
	},
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeOperator {
	PrototypeOf(TypeId),
	PrimitiveTypeName(TypeId),
}

/// TODO instance of?
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeRelationOperator {
	Extends { ty: TypeId, extends: TypeId },
}

// impl<'a> TypeDisplay for TypeId {
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut std::collections::HashSet<usize>,
// 		environment: &GeneralContext,
// 		types: &TypeStore,
// 	) {
// 		use std::fmt::Write;

// 		let (id, this) = (self, types.types[id.0]);

// 		match this {
// 			Type::And(lhs, rhs) | Type::Or(lhs, rhs) => {
// 				TypeDisplay::fmt(lhs, buf, indent, cycles, environment);
// 				buf.push_str(if matches!(this, Type::And(..)) { " & " } else { " | " });
// 				TypeDisplay::fmt(rhs, buf, indent, cycles, environment);
// 			}
// 			Type::NamedRooted { name, .. } => buf.push_str(name),
// 			Type::AliasTo { to, name, parameters } => {
// 				if let Some(name) = name {
// 					buf.push_str(name);
// 					if let Some(parameters) = parameters {
// 						todo!()
// 					}
// 				} else if let Some(constant) = get_ctx!(environment.get_constant_type(*id)) {
// 					match constant {
// 						Constant::Number(num) => write!(buf, "{}", num).unwrap(),
// 						Constant::String(str) => write!(buf, "\"{}\"", str).unwrap(),
// 						Constant::Boolean(val) => write!(buf, "{}", val).unwrap(),
// 						Constant::FunctionReference { .. } => {
// 							let function = get_ctx!(environment.get_function(*id));
// 							if let Some(function) = function {
// 								TypeDisplay::fmt(function, buf, indent, cycles, environment)
// 							} else {
// 								write!(buf, "Unsynthesized function").unwrap();
// 							}
// 						}
// 						Constant::Undefined => write!(buf, "undefined").unwrap(),
// 						Constant::Null => write!(buf, "null").unwrap(),
// 					}
// 				} else if *to == TypeId::ARRAY_TYPE
// 					|| *to == TypeId::OBJECT_TYPE
// 					|| *to == TypeId::TEXT_TYPE
// 					|| *to == TypeId::NODE_LIST_TYPE
// 				{
// 					print_object(*id, buf, environment, indent, cycles);
// 				} else if let Some(function) = get_ctx!(environment.get_function(*id)) {
// 					TypeDisplay::fmt(function, buf, indent, cycles, environment)
// 				} else {
// 					TypeDisplay::fmt(to, buf, indent, cycles, environment);
// 				}
// 			}
// 			Type::RootPolyType { aliases, nature, .. } => {
// 				if let PolyNature::Generic(name) = nature {
// 					buf.push_str(name);
// 				} else {
// 					TypeDisplay::fmt(aliases, buf, indent, cycles, environment);
// 				}
// 			}
// 			Type::Constructor(constructor) => {
// 				// TODO temp
// 				if let Some(Constant::FunctionReference(FunctionPointer::Internal(
// 					internal_function_id,
// 				))) = get_ctx!(environment.get_constant_type(*id))
// 				{
// 					let function = get_ctx!(environment.get_function(*id));
// 					if let Some(function) = function {
// 						TypeDisplay::fmt(function, buf, indent, cycles, environment)
// 					} else {
// 						write!(buf, "Unsynthesized function").unwrap();
// 					}
// 				} else {
// 					match constructor {
// 						Constructor::BinaryOperator { operator, lhs, rhs, .. } => {
// 							buf.push_str("op!");
// 							buf.push('<');
// 							TypeDisplay::fmt(lhs, buf, indent, cycles, environment);
// 							buf.push_str(", ");
// 							TypeDisplay::fmt(rhs, buf, indent, cycles, environment);
// 							buf.push('>');
// 						}
// 						Constructor::UnaryOperator { .. } => todo!(),
// 						Constructor::ConditionalTernary { on, t_res, f_res } => todo!(),
// 						Constructor::StructureGenerics { on, with } => {
// 							TypeDisplay::fmt(on, buf, indent, cycles, environment);
// 							buf.push('<');
// 							for (_, val) in with.iter() {
// 								TypeDisplay::fmt(val, buf, indent, cycles, environment);
// 							}
// 							buf.push('>');
// 						}
// 						Constructor::FunctionResult { on, with: arguments, .. } => {
// 							buf.push('(');
// 							TypeDisplay::fmt(on, buf, indent, cycles, environment);
// 							buf.push(')');
// 							buf.push('(');
// 							for argument in arguments.iter() {
// 								if let SynthesizedArgument::NonSpread { ty: val, .. } = argument {
// 									TypeDisplay::fmt(val, buf, indent, cycles, environment);
// 								}
// 							}
// 							buf.push(')');
// 						}
// 						Constructor::Property { on, under, .. } => {
// 							TypeDisplay::fmt(on, buf, indent, cycles, environment);
// 							buf.push('[');
// 							TypeDisplay::fmt(under, buf, indent, cycles, environment);
// 							buf.push(']');
// 						}
// 						Constructor::Prototype(prototype) => {
// 							buf.push_str("prototype of ");
// 							TypeDisplay::fmt(prototype, buf, indent, cycles, environment);
// 						}
// 						Constructor::RelationOperator { lhs, operator, rhs } => todo!(),
// 						Constructor::LogicalOperator { lhs, operator, rhs } => todo!(),
// 					}
// 				}
// 			}
// 		}
// 	}
// }

fn print_object(
	id: TypeId,
	buf: &mut String,
	environment: &GeneralContext,
	indent: usize,
	cycles: &mut HashSet<usize>,
	types: &TypeStore,
) {
	let ty = types.get_type_by_id(id);
	let property_iterator = get_ctx!(environment.get_properties_on_type(id));

	// TODO needs work
	let is_tuple = matches!(ty, Type::AliasTo { to: TypeId::ARRAY_TYPE, .. });

	// TODO temp
	// if let Type::AliasTo { to: TypeId::TEXT_TYPE, .. } = ty {
	// 	let property =
	// 		get_ctx!(environment.get_property_unbound(id, TypeId::DATA_AS_STRING, types));
	// 	if let Some(TypeLogical::Og(value)) = property {
	// 		todo!();
	// 	// buf.push_str("Text { data: ");
	// 	// TypeDisplay::fmt(&value, buf, indent, cycles, environment);
	// 	// buf.push_str("}");
	// 	// return;
	// 	} else {
	// 		unreachable!()
	// 	}
	// }
	// if let Type::AliasTo { to: TypeId::NODE_LIST_TYPE, .. } = ty {
	// 	// TODO length here
	// 	buf.push_str("NodeList [");
	// 	for (at_end, key) in property_iterator.into_iter().endiate() {
	// 		let value = get_ctx!(environment.get_property_unbound(id, key, types))
	// 			.expect("No property or property constraint");

	// 		let value = match value {
	// 			context::TypeLogical::Og(og) => og,
	// 			context::TypeLogical::Or(_) => todo!(),
	// 			context::TypeLogical::Implies(_, _) => {
	// 				todo!()
	// 			}
	// 		};

	// 		// TypeDisplay::fmt(&value, buf, indent, cycles, environment);
	// 		if !at_end {
	// 			buf.push_str(", ");
	// 		}
	// 	}
	// 	buf.push_str("]");
	// 	return;
	// }

	buf.push_str(if is_tuple { "[" } else { "{ " });

	for (at_end, key) in property_iterator.into_iter().endiate() {
		// let value = get_ctx!(environment.get_property_unbound(id, key, types))
		// 	.expect("No property or property constraint");

		// let value = match value {
		// 	context::TypeLogical::Og(og) => og,
		// 	context::TypeLogical::Or(_) => todo!(),
		// 	context::TypeLogical::Implies(_, _) => {
		// 		todo!()
		// 	}
		// };

		// // TODO getters and setters
		// let is_function = get_ctx!(environment.get_function(value)).is_some();

		// if !is_tuple {
		// 	if let Some(Constant::String(string)) = get_ctx!(environment.get_constant_type(key)) {
		// 		buf.push_str(&string);
		// 	} else {
		// 		buf.push('[');
		// 		TypeDisplay::fmt(&key, buf, indent, cycles, environment);
		// 		buf.push(']');
		// 	}
		// 	if !is_function {
		// 		buf.push_str(": ");
		// 	}
		// } else {
		// 	// TODO assert if is_tuple that it it is in order
		// 	if key == TypeId::LENGTH_AS_STRING {
		// 		continue;
		// 	}
		// }

		// TypeDisplay::fmt(&value, buf, indent, cycles, environment);
		// if !at_end {
		// 	buf.push_str(", ");
		// }
	}

	buf.push_str(if is_tuple { "]" } else { " }" });
}

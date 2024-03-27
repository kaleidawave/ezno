pub mod calling;
pub mod casts;
pub mod classes;
pub mod functions;
pub mod others;
pub mod poly_types;
pub mod printing;
pub mod properties;
pub mod store;
pub mod subtyping;
mod terms;

use derive_debug_extras::DebugExtras;

pub(crate) use poly_types::substitution::*;

pub(crate) use casts::*;
use source_map::SpanWithSource;
pub use store::TypeStore;
pub use terms::Constant;

use crate::{
	context::information::InformationChain,
	events::RootReference,
	features::{
		objects::SpecialObjects,
		operations::{CanonicalEqualityAndInequality, MathematicalAndBitwise, PureUnary},
	},
	types::poly_types::contributions::Contributions,
	Decidable, Environment, FunctionId, SmallMap,
};

pub use self::functions::*;

use self::{
	poly_types::generic_type_arguments::StructureGenericArguments, properties::PropertyKey,
};

pub type ExplicitTypeArgument = (TypeId, SpanWithSource);

/// Final
pub type TypeArguments = SmallMap<TypeId, TypeId>;
pub type TypeRestrictions = SmallMap<TypeId, ExplicitTypeArgument>;
pub type LookUpGenericMap = SmallMap<TypeId, LookUpGeneric>;

/// References [Type]
///
/// TODO maybe [`u32`] or [`u64`]
/// TODO maybe [`crate::SourceId`] like to reference a block, then [`u16`] references some offset
#[derive(PartialEq, Eq, Clone, Copy, DebugExtras, Hash)]
pub struct TypeId(pub(crate) u16);

// TODO ids as macro as to not do in [crate::RootEnvironment]
impl TypeId {
	/// Not to be confused with [`TypeId::NEVER_TYPE`]
	pub const ERROR_TYPE: Self = Self(0);
	pub const UNIMPLEMENTED_ERROR_TYPE: TypeId = TypeId::ERROR_TYPE;

	pub const NEVER_TYPE: Self = Self(1);

	pub const ANY_TYPE: Self = Self(2);

	pub const BOOLEAN_TYPE: Self = Self(3);
	pub const NUMBER_TYPE: Self = Self(4);
	pub const STRING_TYPE: Self = Self(5);

	pub const UNDEFINED_TYPE: Self = Self(6);
	pub const NULL_TYPE: Self = Self(7);
	pub const VOID_TYPE: Self = Self(8);

	/// This is the inner version
	pub const ARRAY_TYPE: Self = Self(9);
	pub const PROMISE_TYPE: Self = Self(10);

	/// Used for Array and Promise
	pub const T_TYPE: Self = Self(11);

	pub const OBJECT_TYPE: Self = Self(12);
	pub const FUNCTION_TYPE: Self = Self(13);
	pub const REGEXP_TYPE: Self = Self(14);
	pub const SYMBOL_TYPE: Self = Self(15);

	/// For more direct stuff and the rules
	pub const TRUE: Self = Self(16);
	pub const FALSE: Self = Self(17);
	pub const ZERO: Self = Self(18);
	pub const ONE: Self = Self(19);
	pub const NAN_TYPE: Self = Self(20);

	/// Shortcut for inferred this
	/// TODO remove
	pub const ANY_INFERRED_FREE_THIS: Self = Self(21);

	/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target>
	pub const NEW_TARGET_ARG: Self = Self(22);

	pub const SYMBOL_TO_PRIMITIVE: Self = Self(23);

	pub const CONSTANT_RESTRICTION: Self = Self(24);

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
	And(TypeId, TypeId),
	Or(TypeId, TypeId),
	RootPolyType(PolyNature),
	/// Also a "Substituted constructor type"
	Constructor(Constructor),
	/// For number and other rooted types
	///
	/// Although they all alias Object
	Interface {
		name: String,
		// Whether only values under this type can be matched
		nominal: bool,
		parameters: Option<Vec<TypeId>>,
	},
	/// Pretty much same as [`Type::Interface`]
	Class {
		name: String,
		parameters: Option<Vec<TypeId>>,
	},
	/// *Dependent equality types*
	Constant(crate::Constant),

	/// From a type annotation or .d.ts WITHOUT body. e.g. don't know effects TODO...
	FunctionReference(FunctionId),

	/// Technically could be just a function but...
	Object(ObjectNature),
	SpecialObject(SpecialObjects),
}

/// TODO difference between untyped and typed parameters and what about parameter based for any
///
/// Most of the difference here is just for debugging, printing etc
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum PolyNature {
	Parameter {
		fixed_to: TypeId,
	},
	StructureGeneric {
		name: String,
		constrained: bool,
	},
	FunctionGeneric {
		name: String,
		eager_fixed: TypeId,
	},
	Open(TypeId),
	/// For functions and for loops where something in the scope can mutate
	/// (so not constant) between runs.
	FreeVariable {
		reference: RootReference,
		based_on: TypeId,
	},
	RecursiveFunction(FunctionId, TypeId),
}

impl PolyNature {
	pub fn is_substitutable(&self) -> bool {
		matches!(
			self,
			Self::Parameter { .. } | Self::StructureGeneric { .. } | Self::FunctionGeneric { .. }
		)
	}
}

// TODO
#[must_use]
pub fn is_primitive(ty: TypeId, _types: &TypeStore) -> bool {
	if matches!(ty, TypeId::BOOLEAN_TYPE | TypeId::NUMBER_TYPE | TypeId::STRING_TYPE) {
		return true;
	}
	false
}

/// not full constant but
#[must_use]
pub fn is_type_constant(ty: TypeId, types: &TypeStore) -> bool {
	// TODO `Type::SpecialObject(..)` might cause issues
	matches!(ty, TypeId::UNDEFINED_TYPE | TypeId::NULL_TYPE)
		|| matches!(
			types.get_type_by_id(ty),
			Type::Constant(..) | Type::Object(ObjectNature::RealDeal) | Type::SpecialObject(..)
		)
}

/// TODO split
#[derive(Copy, Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum ObjectNature {
	/// Actual allocated object
	RealDeal,
	/// From `x: { a: number }` etc
	AnonymousTypeAnnotation,
}

impl Type {
	pub(crate) fn get_parameters(&self) -> Option<Vec<TypeId>> {
		if let Type::Class { parameters, .. }
		| Type::Interface { parameters, .. }
		| Type::AliasTo { parameters, .. } = self
		{
			parameters.clone()
		} else {
			None
		}
	}

	/// TODO return is poly
	pub(crate) fn is_dependent(&self) -> bool {
		#[allow(clippy::match_same_arms)]
		match self {
			// TODO
			Type::Constructor(Constructor::StructureGenerics(..)) => false,
			// Fine
			Type::Constructor(_) | Type::RootPolyType(_) => true,
			// TODO what about if left or right
			Type::And(_, _) | Type::Or(_, _) => false,
			// TODO what about if it aliases dependent?
			Type::AliasTo { .. } => false,
			Type::Interface { .. } | Type::Class { .. } => false,
			Type::Constant(_)
			| Type::SpecialObject(_)
			| Type::FunctionReference(..)
			| Type::Object(_) => false,
		}
	}

	pub(crate) fn is_operator(&self) -> bool {
		matches!(self, Self::And(..) | Self::Or(..))
	}

	fn is_nominal(&self) -> bool {
		matches!(self, Self::Class { .. } | Self::Interface { nominal: true, .. })
	}
}

/// - Note that no || etc. This is handled using [`Constructor::ConditionalResult`]
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum Constructor {
	// TODO separate add?
	BinaryOperator {
		lhs: TypeId,
		operator: MathematicalAndBitwise,
		rhs: TypeId,
	},
	CanonicalRelationOperator {
		lhs: TypeId,
		operator: CanonicalEqualityAndInequality,
		rhs: TypeId,
	},
	UnaryOperator {
		operator: PureUnary,
		operand: TypeId,
	},
	TypeOperator(TypeOperator),
	TypeRelationOperator(TypeRelationOperator),
	/// TODO constraint is res
	ConditionalResult {
		/// TODO this can only be poly types
		condition: TypeId,
		truthy_result: TypeId,
		otherwise_result: TypeId,
		result_union: TypeId,
	},
	/// output of a function where on is dependent (or sometimes for const functions one of `with` is dependent)
	Image {
		on: TypeId,
		// TODO I don't think this is necessary, maybe for debugging. In such case should be an Rc to share with events
		with: Box<[SynthesisedArgument]>,
		result: TypeId,
	},
	/// Access
	Property {
		on: TypeId,
		under: PropertyKey<'static>,
		result: TypeId,
		bind_this: bool,
	},
	/// For await a poly type
	Awaited {
		on: TypeId,
		result: TypeId,
	},
	/// **e.g `Array<string>`
	/// Might not be best place but okay.
	StructureGenerics(StructureGenerics),
}

impl Constructor {
	fn is_structure_generics(&self) -> bool {
		matches!(self, Constructor::StructureGenerics(..))
	}

	fn get_base(&self) -> Option<TypeId> {
		match self {
			Constructor::ConditionalResult { result_union: result, .. }
			| Constructor::Awaited { result, .. }
			| Constructor::Image { result, .. } => Some(*result),
			Constructor::BinaryOperator { .. }
			| Constructor::CanonicalRelationOperator { .. }
			| Constructor::UnaryOperator { .. }
			| Constructor::TypeOperator(_)
			| Constructor::TypeRelationOperator(_)
			| Constructor::Property { .. }
			| Constructor::StructureGenerics(_) => None,
		}
	}
}

/// Closed over arguments
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct StructureGenerics {
	pub on: TypeId,
	pub arguments: StructureGenericArguments,
}

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeOperator {
	/// Gets the prototype
	PrototypeOf(TypeId),
	/// The `typeof` unary operator
	TypeOf(TypeId),
}

/// TODO instance of?
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum TypeRelationOperator {
	Extends { ty: TypeId, extends: TypeId },
}

pub(crate) fn new_logical_or_type(lhs: TypeId, rhs: TypeId, types: &mut TypeStore) -> TypeId {
	types.new_conditional_type(lhs, lhs, rhs)
}

#[must_use]
pub fn is_type_truthy_falsy(id: TypeId, types: &TypeStore) -> Decidable<bool> {
	// These first two branches are just shortcuts.
	if id == TypeId::TRUE || id == TypeId::FALSE {
		Decidable::Known(id == TypeId::TRUE)
	} else if id == TypeId::NULL_TYPE || id == TypeId::UNDEFINED_TYPE {
		Decidable::Known(false)
	} else {
		let ty = types.get_type_by_id(id);
		match ty {
			Type::AliasTo { .. }
			| Type::And(_, _)
			| Type::Or(_, _)
			| Type::RootPolyType(_)
			| Type::Constructor(_)
			| Type::Interface { .. }
			| Type::Class { .. } => {
				// TODO some of these case are known
				Decidable::Unknown(id)
			}
			Type::FunctionReference(..) | Type::SpecialObject(_) | Type::Object(_) => {
				Decidable::Known(true)
			}
			Type::Constant(cst) => {
				// TODO strict casts
				Decidable::Known(cast_as_boolean(cst, false).unwrap())
			}
		}
	}
}

/// TODO `add_property_restrictions` via const generics
pub struct BasicEquality {
	pub position: SpanWithSource,
	pub add_property_restrictions: bool,
	pub object_constraints: Vec<(TypeId, TypeId)>,
	/// used in `satisfies` assertions for the unit tests lol
	pub allow_errors: bool,
}

/// For subtyping

pub trait SubTypeBehavior<'a> {
	/// For specialisation during calling
	fn get_contributions<'b>(&'b mut self) -> Option<&'b mut Contributions<'a>>;

	fn add_object_mutation_constraint(&mut self, on: TypeId, constraint: TypeId);

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	);

	fn allow_errors(&self) -> bool;

	// /// TODO can go faster than Vec, by passing all options,
	// fn get_constraint<C: InformationChain>(&self, under: TypeId, info: &C) -> Option<ArgumentOrLookup>;
}

pub enum ArgumentOrLookup {
	Argument(TypeId),
	LookUpGeneric(LookUpGeneric),
}

impl<'a> SubTypeBehavior<'a> for BasicEquality {
	fn get_contributions<'b>(&'b mut self) -> Option<&'b mut Contributions<'a>> {
		None
	}

	fn add_object_mutation_constraint(&mut self, on: TypeId, constraint: TypeId) {
		self.object_constraints.push((on, constraint));
	}

	fn add_function_restriction(
		&mut self,
		environment: &mut Environment,
		function_id: FunctionId,
		function_type: FunctionType,
	) {
		let result = environment
			.deferred_function_constraints
			.insert(function_id, (function_type, self.position));

		debug_assert!(result.is_none());
	}

	fn allow_errors(&self) -> bool {
		self.allow_errors
	}
}

/// TODO implement `Try` / `?` on `SubTypeResult`
#[derive(Debug)]
pub enum SubTypeResult {
	IsSubType,
	IsNotSubType(NonEqualityReason),
}

pub type GenericChain<'a> = Option<GenericChainLink<'a>>;
pub type GenericChainParent<'a> = Option<&'a GenericChainLink<'a>>;

/// - Used for printing and subtyping. Handles nested restrictions
/// - Uses lifetimes because lifetimes
#[derive(Clone, Copy, Debug)]
pub enum GenericChainLink<'a> {
	Link {
		parent: GenericChainParent<'a>,
		value: &'a StructureGenericArguments,
	},
	FunctionRoot {
		parent: Option<&'a StructureGenericArguments>,
		call_site_type_arguments: Option<&'a map_vec::Map<TypeId, (TypeId, SpanWithSource)>>,
		type_arguments: &'a map_vec::Map<TypeId, TypeId>,
	},
}

impl<'a> GenericChainLink<'a> {
	fn _get_value(self) -> Option<&'a StructureGenericArguments> {
		if let Self::Link { value, .. } = self {
			Some(value)
		} else {
			None
		}
	}

	/// TODO wip
	#[allow(unused)]
	pub(crate) fn get_argument(
		&self,
		on: TypeId,
		info: &impl InformationChain,
		types: &TypeStore,
	) -> Option<Vec<TypeId>> {
		match self {
			GenericChainLink::Link { parent, value } => value
				.get_argument_as_list(on, info, types)
				.or_else(|| parent.and_then(|parent| parent.get_argument(on, info, types))),
			GenericChainLink::FunctionRoot { parent, call_site_type_arguments, type_arguments } => {
				parent
					.and_then(|parent| parent.get_argument_as_list(on, info, types))
					.or_else(|| {
						call_site_type_arguments
							.and_then(|ta1| ta1.get(&on).map(|(arg, _)| vec![*arg]))
					})
					.or_else(|| type_arguments.get(&on).map(|a| vec![*a]))
			}
		}
	}

	pub(crate) fn append_to_link(
		parent: GenericChainParent<'a>,
		value: &'a StructureGenericArguments,
	) -> GenericChainLink<'a> {
		GenericChainLink::Link { parent, value }
	}

	pub(crate) fn append(
		parent: GenericChainParent<'a>,
		value: &'a StructureGenericArguments,
	) -> GenericChain<'a> {
		Some(GenericChainLink::append_to_link(parent, value))
	}

	/// Does not do 'lookup generics'. Which may be fine
	///
	/// - (swaps `get_argument_as_list` with `get_structure_restriction`)
	pub(crate) fn get_single_argument(&self, on: TypeId) -> Option<TypeId> {
		match self {
			GenericChainLink::Link { parent, value } => value
				.get_structure_restriction(on)
				.or_else(|| parent.and_then(|parent| parent.get_single_argument(on))),
			GenericChainLink::FunctionRoot { parent, call_site_type_arguments, type_arguments } => {
				parent
					.and_then(|parent| parent.get_structure_restriction(on))
					.or_else(|| {
						call_site_type_arguments.and_then(|ta1| ta1.get(&on).map(|(arg, _)| *arg))
					})
					.or_else(|| type_arguments.get(&on).copied())
			}
		}
	}
}

// TODO maybe positions and extra information here
// SomeLiteralMismatch
// GenericParameterCollision
#[derive(Debug)]
pub enum NonEqualityReason {
	Mismatch,
	PropertiesInvalid {
		errors: Vec<(PropertyKey<'static>, PropertyError)>,
	},
	TooStrict,
	/// TODO more information
	MissingParameter,
	GenericParameterMismatch,
}

#[derive(Debug)]
pub enum PropertyError {
	Missing,
	Invalid { expected: TypeId, found: TypeId, mismatch: NonEqualityReason },
}

/// TODO temp fix for printing
pub(crate) fn is_explicit_generic(on: TypeId, types: &TypeStore) -> bool {
	if let Type::RootPolyType(PolyNature::FunctionGeneric { .. }) = types.get_type_by_id(on) {
		true
	} else if let Type::Constructor(Constructor::Property { on, under, result: _, bind_this: _ }) =
		types.get_type_by_id(on)
	{
		is_explicit_generic(*on, types)
			|| matches!(under, PropertyKey::Type(under) if is_explicit_generic(*under, types))
	} else {
		false
	}
}

/// Finds the constraint of poly types
///
/// **Also looks at possibly mutated things
pub(crate) fn get_constraint(on: TypeId, types: &TypeStore) -> Option<TypeId> {
	match types.get_type_by_id(on) {
		Type::RootPolyType(nature) => Some(
			*(match nature {
				PolyNature::Parameter { fixed_to } => fixed_to,
				PolyNature::FunctionGeneric { name: _, eager_fixed } => eager_fixed,
				PolyNature::Open(ty) => ty,
				PolyNature::FreeVariable { reference: _, based_on } => based_on,
				PolyNature::RecursiveFunction(_, return_ty) => return_ty,
				PolyNature::StructureGeneric { constrained, .. } => {
					return if *constrained {
						todo!("get from TypeStore or ???")
					} else {
						Some(TypeId::ANY_TYPE)
					}
				}
			}),
		),
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { lhs, operator, rhs } => {
				if let MathematicalAndBitwise::Add = operator {
					let lhs = get_larger_type(lhs, types);
					let rhs = get_larger_type(rhs, types);
					// TODO these need to be generated
					if let (TypeId::NUMBER_TYPE, TypeId::NUMBER_TYPE) = (lhs, rhs) {
						Some(TypeId::NUMBER_TYPE)
					} else if let (TypeId::STRING_TYPE, _) | (_, TypeId::STRING_TYPE) = (lhs, rhs) {
						Some(TypeId::STRING_TYPE)
					} else {
						crate::utils::notify!("lhs = {:?}", types.get_type_by_id(lhs));
						crate::utils::notify!("TODO use existing conditional");
						Some(TypeId::NUMBER_TYPE)
					}
				} else {
					Some(TypeId::NUMBER_TYPE)
				}
			}
			Constructor::UnaryOperator { operand: _, operator: _ } => {
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
			Constructor::Image { on: _, with: _, result } => Some(result),
			Constructor::Property { on: _, under: _, result, bind_this: _ } => {
				crate::utils::notify!("Here, result of a property get");
				Some(result)
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
				crate::types::TypeRelationOperator::Extends { .. } => Some(TypeId::BOOLEAN_TYPE),
			},
			// TODO sure?
			Constructor::StructureGenerics { .. } => None,
			Constructor::Awaited { on: _, result } => Some(result),
		},
		Type::Object(ObjectNature::RealDeal) => {
			// crate::utils::notify!("Might be missing some mutations that are possible here");
			None
		}
		_ => None,
	}
}

fn get_larger_type(on: TypeId, types: &TypeStore) -> TypeId {
	if let Some(poly_base) = get_constraint(on, types) {
		poly_base
	} else if let Type::Constant(cst) = types.get_type_by_id(on) {
		cst.get_backing_type_id()
	} else {
		on
	}
}

/// TODO T on `Array`, U, V on `Map` etc. Works around not having `&mut TypeStore` and mutations in-between
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub enum LookUpGeneric {
	NumberPropertyOfSelf, // Property(Box<Self>, PropertyKey<'static>),
}

impl LookUpGeneric {
	#[allow(unreachable_patterns)]
	pub(crate) fn calculate_lookup(&self, info: &impl InformationChain, on: TypeId) -> Vec<TypeId> {
		match self {
			LookUpGeneric::NumberPropertyOfSelf => {
				info.get_chain_of_info()
					.filter_map(|info| info.current_properties.get(&on).map(|v| v.iter()))
					.flatten()
					.filter_map(|(_publicity, key, value)| {
						// TODO filter more
						if matches!(key, PropertyKey::String(s) if s == "length") {
							None
						} else {
							Some(value.as_get_type())
						}
					})
					.collect()
			}
			_ => unreachable!(), // LookUpGeneric::Property(_, _) => todo!(),
		}
	}
}

pub trait TypeCombinable {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		otherwise_result: Self,
		types: &mut TypeStore,
	) -> Self;

	fn default() -> Self;
}

// For if-else branches
impl TypeCombinable for () {
	fn combine(
		_condition: TypeId,
		_truthy_result: Self,
		_otherwise_result: Self,
		_types: &mut TypeStore,
	) -> Self {
	}

	fn default() -> Self {}
}

// For ternary conditional operators
impl TypeCombinable for TypeId {
	fn combine(
		condition: TypeId,
		truthy_result: Self,
		otherwise_result: Self,
		types: &mut TypeStore,
	) -> Self {
		types.new_conditional_type(condition, truthy_result, otherwise_result)
	}

	fn default() -> Self {
		TypeId::UNDEFINED_TYPE
	}
}

/// Used for **both** inference and narrowing
pub enum Confirmation {
	HasProperty { on: (), property: () },
	IsType { on: (), ty: () },
}

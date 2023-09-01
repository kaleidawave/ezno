use std::collections::HashMap;

use source_map::Span;

use crate::{events::RootReference, FunctionId, GenericTypeParameters, TypeId};

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

	/// TODO temp
	pub kind: FunctionKind,

	pub id: FunctionId,
}

/// TODO as generics
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum FunctionKind {
	Arrow,
	Function {
		function_prototype: TypeId,
	},
	ClassConstructor {
		// TODO constructor event
	},
	Method,
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

/// Optionality is indicated by what vector it is in [SynthesizedParameters]
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesizedParameter {
	pub name: String,
	/// This is the generic parameter type, not the restriction
	pub ty: TypeId,
	pub position: Span,
	/// For optional parameters this is [TypeId::UNDEFINED_TYPE] else some type
	pub missing_value: Option<TypeId>,
}

/// **Note that the [Type] here is not array like**
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesizedRestParameter {
	pub name: String,
	/// This is the T, of Array<T>
	pub item_type: TypeId,
	pub position: Span,
}

/// A type of a collection of function parameters
///
/// This applies for source functions
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct SynthesizedParameters {
	// Even though these vectors are the same type, the latter allows for elided arguments
	pub parameters: Vec<SynthesizedParameter>,
	pub rest_parameter: Option<SynthesizedRestParameter>,
}

// impl TypeDisplay for SynthesizedParameters {
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut std::collections::HashSet<usize>,
// 		environment: &GeneralContext,
// 		store: &TypeStore,
// 	) {
// 		buf.push('(');

// 		for (at_end, SynthesizedParameter { name, ty: constraint, .. }) in
// 			self.parameters.iter().endiate()
// 		{
// 			buf.push_str(name);
// 			buf.push_str(": ");
// 			let constraint_ty = get_on_ctx!(environment.get_type_by_id(*constraint));
// 			TypeDisplay::fmt(constraint, buf, indent, cycles, environment, store);
// 			if !at_end || !self.optional_parameters.is_empty() || self.rest_parameter.is_some() {
// 				buf.push_str(", ");
// 			}
// 		}

// 		for (at_end, SynthesizedParameter { name, ty: constraint, .. }) in
// 			self.optional_parameters.iter().endiate()
// 		{
// 			buf.push_str(name);
// 			buf.push_str("?: ");
// 			let constraint_ty = get_on_ctx!(environment.get_type_by_id(*constraint));
// 			TypeDisplay::fmt(constraint, buf, indent, cycles, environment);
// 			if !at_end || self.rest_parameter.is_some() {
// 				buf.push_str(", ");
// 			}
// 		}

// 		if let Some(SynthesizedRestParameter { name, item_type, .. }) = self.rest_parameter.as_ref()
// 		{
// 			buf.push_str("...");
// 			buf.push_str(&name);
// 			buf.push_str(": ");
// 			let item_type_ty = get_on_ctx!(environment.get_type_by_id(*item_type));
// 			TypeDisplay::fmt(item_type, buf, indent, cycles, environment);
// 		}

// 		buf.push(')')
// 	}
// }

impl SynthesizedParameters {
	// TODO should be aware of undefined in optionals possibly
	pub(crate) fn get_type_constraint_at_index(&self, idx: usize) -> Option<TypeId> {
		if let Some(ref param) = self.parameters.get(idx) {
			Some(param.ty)
		} else if let Some(ref rest) = self.rest_parameter {
			Some(rest.item_type)
		} else {
			None
		}
	}
}

/// TODO spread should of tuples should expand into `NonSpread`
/// TODO spread for non heterogenous things
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
#[non_exhaustive]
pub enum SynthesizedArgument {
	/// This is the get value of a argument
	NonSpread { ty: TypeId, position: Span },
	// TODO
	// Spread(Instance),
}

impl SynthesizedArgument {
	pub(crate) fn get_position(&self) -> Span {
		match self {
			SynthesizedArgument::NonSpread { ty: _, position } => position.clone(),
		}
	}

	pub(crate) fn into_type(&self) -> Result<TypeId, ()> {
		match self {
			SynthesizedArgument::NonSpread { ty, position: _ } => Ok(*ty),
		}
	}
}

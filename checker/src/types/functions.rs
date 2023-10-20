use std::collections::HashMap;

use source_map::{Span, SpanWithSource};

use crate::{events::RootReference, FunctionId, GenericTypeParameters, TypeId};

#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct FunctionType {
	/// Syntax defined pointer
	pub id: FunctionId,

	/// TODO not sure about this field and how it tails with Pi Types
	pub type_parameters: Option<GenericTypeParameters>,
	pub parameters: SynthesisedParameters,
	pub return_type: TypeId,
	/// Side effects of the function
	pub effects: Vec<crate::events::Event>,

	/// Things that this function pulls in. Converse of closed over which is where results below use
	/// variables in this scope.
	pub free_variables: HashMap<RootReference, TypeId>,

	/// References it needs to retain for returning / other effects where things go out.
	///
	/// The type is the initial value of the closure variable when this is called
	pub closed_over_variables: HashMap<RootReference, TypeId>,

	/// Can be called for constant result
	pub constant_id: Option<String>,

	/// TODO temp
	pub kind: FunctionKind,
}

/// TODO temp
#[derive(Clone, Copy, Debug, binary_serialize_derive::BinarySerializable)]
pub enum GetSet {
	Get,
	Set,
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
	Method {
		/// TODO this should be generically rather than at runtime
		get_set: Option<GetSet>,
	},
}

// /// TODO needs improvement
// #[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
// pub enum FunctionNature {
// 	BehindPoly {
// 		/// TODO function id?
// 		function_id_if_open_poly: Option<FunctionId>,
// 		state: FunctionState,
// 	},
// 	/// Last is 'this' type,
// 	Source {
// 		state: FunctionState,
// 	},
// 	Reference,
// }

// impl FunctionNature {
// 	pub fn get_this_and_closed_variables(self) -> (Option<TypeId>, ClosedOverVariables) {
// 		match self {
// 			FunctionNature::BehindPoly { function_id_if_open_poly: _ , state }
// 			| FunctionNature::Source { state } => (state.value_of_this, state.closed_over_variables),
// 			FunctionNature::Reference => (None, Default::default()),
// 		}
// 	}
// }

/// Optionality is indicated by what vector it is in [SynthesisedParameters]
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedParameter {
	pub name: String,
	/// This is the generic parameter type, not the restriction
	pub ty: TypeId,
	pub position: SpanWithSource,
	/// For optional parameters this is [TypeId::UNDEFINED_TYPE] else some type
	pub missing_value: Option<TypeId>,
}

/// **Note that the [Type] here is not array like**
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedRestParameter {
	pub name: String,
	/// This is the T, of Array<T>
	pub item_type: TypeId,
	pub position: SpanWithSource,
}

/// A type of a collection of function parameters
///
/// This applies for source functions
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct SynthesisedParameters {
	// Even though these vectors are the same type, the latter allows for elided arguments
	pub parameters: Vec<SynthesisedParameter>,
	pub rest_parameter: Option<SynthesisedRestParameter>,
}

// impl TypeDisplay for SynthesisedParameters {
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut std::collections::HashSet<usize>,
// 		environment: &GeneralContext,
// 		store: &TypeStore,
// 	) {
// 		buf.push('(');

// 		for (at_end, SynthesisedParameter { name, ty: constraint, .. }) in
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

// 		for (at_end, SynthesisedParameter { name, ty: constraint, .. }) in
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

// 		if let Some(synthesisedRestParameter { name, item_type, .. }) = self.rest_parameter.as_ref()
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

impl SynthesisedParameters {
	// TODO should be aware of undefined in optionals possibly
	pub(crate) fn get_type_constraint_at_index(&self, idx: usize) -> Option<TypeId> {
		if let Some(param) = self.parameters.get(idx) {
			Some(param.ty)
		} else {
			self.rest_parameter.as_ref().map(|rest| rest.item_type)
		}
	}
}

/// TODO spread should of tuples should expand into `NonSpread`
/// TODO spread for non heterogenous things
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
#[non_exhaustive]
pub enum SynthesisedArgument {
	/// This is the get value of a argument
	NonSpread { ty: TypeId, position: SpanWithSource },
	// TODO
	// Spread(Instance),
}

impl SynthesisedArgument {
	pub(crate) fn get_position(&self) -> SpanWithSource {
		match self {
			SynthesisedArgument::NonSpread { ty: _, position } => position.clone(),
		}
	}

	pub(crate) fn into_type(&self) -> Result<TypeId, ()> {
		match self {
			SynthesisedArgument::NonSpread { ty, position: _ } => Ok(*ty),
		}
	}
}

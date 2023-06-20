use source_map::Span;

use crate::TypeId;

/// Optionality is indicated by what vector it is in [SynthesizedParameters]
#[derive(Clone, Debug, binary_serialize_derive::BinarySerializable)]
pub struct SynthesizedParameter {
	pub name: String,
	pub ty: TypeId,
	pub position: Span,
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
	pub optional_parameters: Vec<SynthesizedParameter>,
	pub rest_parameter: Option<SynthesizedRestParameter>,
}

// impl TypeDisplay for SynthesizedParameters {
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut std::collections::HashSet<usize>,
// 		environment: &GeneralEnvironment,
// 		store: &TypeStore,
// 	) {
// 		buf.push('(');

// 		for (at_end, SynthesizedParameter { name, ty: constraint, .. }) in
// 			self.parameters.iter().endiate()
// 		{
// 			buf.push_str(name);
// 			buf.push_str(": ");
// 			let constraint_ty = get_env!(environment.get_type_by_id(*constraint));
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
// 			let constraint_ty = get_env!(environment.get_type_by_id(*constraint));
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
// 			let item_type_ty = get_env!(environment.get_type_by_id(*item_type));
// 			TypeDisplay::fmt(item_type, buf, indent, cycles, environment);
// 		}

// 		buf.push(')')
// 	}
// }

impl SynthesizedParameters {
	// TODO should be aware of undefined in optionals possibly
	pub(crate) fn get_type_constraint_at_index(&self, idx: usize) -> Option<TypeId> {
		self.parameters
			.get(idx)
			.map(|param| param.ty)
			.or_else(|| {
				self.optional_parameters.get(self.parameters.len() + idx).map(|param| param.ty)
			})
			.or(self.rest_parameter.as_ref().map(|param| param.item_type))
	}
}

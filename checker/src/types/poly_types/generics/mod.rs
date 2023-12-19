//! Generics = dependent function types

pub mod generic_structure;
pub mod generic_type_arguments;
pub mod generic_type_parameters;

pub use generic_structure::*;
pub(crate) use generic_type_arguments::FunctionTypeArguments;
pub use generic_type_parameters::*;
use source_map::SpanWithSource;

use crate::{types::FunctionType, FunctionId, TypeId};

/// When comparing parameters to arguments this sets type arguments
pub(crate) struct SeedingContext {
	/// usize = argument position
	pub type_arguments: map_vec::Map<TypeId, Vec<(TypeId, SpanWithSource, usize)>>,
	/// Produced by explicit call site type arguments (TODO explain) and function parameters (TODO explain)
	pub type_restrictions: map_vec::Map<TypeId, Vec<(TypeId, SpanWithSource)>>,

	/// For functions that are called but are recursive etc...
	pub locally_held_functions: map_vec::Map<FunctionId, FunctionType>,

	/// TODO temp, needs to be passed through somehow
	pub argument_position_and_parameter_idx: (SpanWithSource, usize),
}

impl SeedingContext {
	pub(crate) fn set_id(
		&mut self,
		on: TypeId,
		arg: (TypeId, SpanWithSource, usize),
		restriction: bool,
	) {
		// crate::utils::notify!(
		// 	"Setting argument {:?} to {:?}",
		// 	_ts.debug_type(on),
		// 	_ts.debug_type(arg)
		// );

		if restriction {
			let (arg, pos, _) = arg;
			if let Some(args) = self.type_restrictions.get_mut(&on) {
				args.push((arg, pos));
			} else {
				self.type_restrictions.insert(on, vec![(arg, pos)]);
			}
		} else if let Some(args) = self.type_arguments.get_mut(&on) {
			args.push(arg);
		} else {
			self.type_arguments.insert(on, vec![arg]);
		};
	}
}

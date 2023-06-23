use std::{
	collections::{HashMap, HashSet},
	path::PathBuf,
};

use source_map::Span;

use crate::{
	structures::variables::VariableWithValue,
	types::{printing::print_type, TypeId, TypeStore},
	FunctionId, GeneralContext, Variable, VariableId,
};

/// TODO temp
#[derive(PartialEq, Eq, Debug)]
pub struct HashableSpan(pub Span);

impl From<Span> for HashableSpan {
	fn from(item: Span) -> Self {
		HashableSpan(item)
	}
}

impl std::hash::Hash for HashableSpan {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.0.start.hash(state);
		self.0.end.hash(state);
	}
}

/// [TypeMappings] is used to retaining information between passes, including the synthesize and checking passes
/// This for use in the both use in the compiler and compiler plugins
/// Checking things are held on [crate::Memory], function things are held on [crate::HoistedFunctionContainer]
/// and module things on [crate::ModuleData]
///
/// TODO Span as keys doesn't work, need a better data-structure
#[derive(Default, Debug)]
pub struct TypeMappings {
	/// Figures out the types of the expressions in the AST
	pub expressions_to_instances: HashMap<HashableSpan, Instance>,
	/// [Variable] data to a AST mapping
	pub variables_to_variables: HashMap<HashableSpan, Variable>,
	/// Property to type, TODO kind of temp
	pub properties_to_types: HashMap<HashableSpan, TypeId>,
	/// Data to a AST mapping. For classes this points to the shape
	pub types_to_types: HashMap<HashableSpan, TypeId>,
	pub import_statements_to_pointing_path: HashMap<HashableSpan, PathBuf>,
	/// can be used for tree shaking
	pub called_functions: HashSet<FunctionId>,
}

// TODO these are temp
impl TypeMappings {
	pub fn print_called_functions(&self, source: &str) -> String {
		let mut buf = "Called functions:\n".to_owned();
		for func_id in self.called_functions.iter() {
			if *func_id != FunctionId::NULL {
				buf.push_str(
					source
						.get((func_id.0.start as usize)..(func_id.0.end as usize))
						.unwrap_or_default(),
				);
				buf.push('\n')
			}
		}
		buf
	}

	pub fn print_type_mappings(
		&self,
		source: &str,
		env: &GeneralContext,
		types: &TypeStore,
	) -> String {
		let mut buf = "Expression type mappings:\n".to_owned();
		for (pos, instance) in self.expressions_to_instances.iter() {
			buf.push_str(
				source.get((pos.0.start as usize)..(pos.0.end as usize)).unwrap_or_default(),
			);
			buf.push_str(" has type ");
			buf.push_str(&print_type(types, instance.get_value(), env));
			buf.push('\n')
		}
		buf
	}
}

/// See https://www.internalpointers.com/post/understanding-meaning-lexpressions-and-rexpressions-c for a understanding
/// of LValue vs RValue
#[derive(Clone, Debug)]
pub enum Instance {
	LValue(VariableWithValue),
	RValue(TypeId),
	/// Result FROM getter
	GValue(TypeId),
}

impl Instance {
	pub fn get_variable_id(&self) -> Option<VariableId> {
		match self {
			Self::LValue(variable) => Some(variable.0.get_id()),
			Self::RValue(_) | Self::GValue(_) => None,
		}
	}

	pub fn get_value(&self) -> TypeId {
		match self {
			Instance::LValue(l) => l.1,
			Instance::GValue(value) | Instance::RValue(value) => *value,
		}
	}
}

use std::{collections::HashMap, path::PathBuf};

use source_map::Span;

use crate::{
	context::VariableId, structures::variables::VariableWithValue, types::TypeId, FunctionPointer,
	JSXComponentReference, Variable,
};

/// [TypeMappings] is used to retaining information between passes, including the synthesize and checking passes
/// This for use in the both use in the compiler and compiler plugins
/// Checking things are held on [crate::Memory], function things are held on [crate::HoistedFunctionContainer]
/// and module things on [crate::ModuleData]
///
/// TODO Span as keys doesn't work, need a better data-structure
#[derive(Default, Debug)]
pub struct TypeMappings {
	/// Figures out the types of the expressions in the AST
	pub expressions_to_instances: HashMap<Span, Instance>,
	/// [Variable] data to a AST mapping
	pub variables_to_variables: HashMap<Span, Variable>,
	/// Property to type, TODO kind of temp
	pub properties_to_types: HashMap<Span, TypeId>,
	/// Data to a AST mapping. For classes this points to the shape
	pub types_to_types: HashMap<Span, TypeId>,
	/// Returns the underlying type of a component referenced in JSX. etc whether <X> is a
	/// function or a class. Contains the [VariableId] of said instance
	pub jsx_element_map: HashMap<Span, JSXComponentReference>,
	pub import_statements_to_pointing_path: HashMap<Span, PathBuf>,
	/// TODO this is very similar to types to types apart from StatementFunctions do not have parser::TypeId
	pub functions_to_prototypes: HashMap<Span, TypeId>,
	/// TODO very similar to above
	pub classes_to_constructors: HashMap<Span, TypeId>,
	/// For for [FunctionPointer::Internal] this is the declaration in a module or definition
	pub functions_to_positions: HashMap<FunctionPointer, Span>,
}

impl TypeMappings {
	/// Optional as new AST with new ExpressionId may not have added to the map
	/// TODO funny function :0
	/// TODO maybe return a error instance if cannot find instance for expression
	pub fn get_instance_for_expression(&self, expression_id: &Span) -> Option<&Instance> {
		todo!()
		// self.expressions_to_instances.get(expression_id)
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

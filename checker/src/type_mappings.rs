use crate::{
	features::variables::VariableWithValue,
	types::{TypeId, TypeStore},
	GeneralContext, RangeMap, VariableId,
};
use source_map::{SourceId, SpanWithSource};
use std::{collections::HashMap, path::PathBuf};

/// **PER MODULE**
/// [`TypeMappings`] is used to retaining information between passes, including the synthesise and checking passes
/// This for use in the both use in the compiler and compiler plugins
/// Checking things are held on [`crate::Memory`], function things are held on [`crate::HoistedFunctionContainer`]
/// and module things on [`crate::ModuleData`]
#[derive(Default, Debug)]
pub struct TypeMappings {
	/// Figures out the types of the expressions in the AST
	pub expressions_to_instances: RangeMap<Instance>,
	/// [Variable] data to a AST mapping.
	/// **THIS INFORMATION IS CURRENTLY USED IN TYPE CHECKING AND SO IS NOT JUST FOR LATER ANALYSIS**
	pub variables_to_constraints: VariablesToTypes,
	/// Property to type, TODO kind of temp
	pub properties_to_types: RangeMap<TypeId>,
	/// Data to a AST mapping. For classes this points to the shape
	pub types_to_types: RangeMap<TypeId>,
	pub import_statements_to_pointing_path: RangeMap<PathBuf>,

	/// For places where there are two `var` statements in
	pub var_aliases: HashMap<u32, VariableId>,

	/// Variable restriction. Cached after hoisting pass. TODO temp needs tidy
	pub variable_restrictions: HashMap<(SourceId, u32), (TypeId, SpanWithSource)>,
	/// Temp
	pub special_expressions: RangeMap<SpecialExpressions>,
}

#[derive(Debug)]
pub enum SpecialExpressions {
	CompileOut,
	Marker,
}

#[derive(Default, Debug)]
pub struct VariablesToTypes(pub(crate) HashMap<VariableId, TypeId>);

// TODO these are temp
impl TypeMappings {
	#[must_use]
	pub fn print_type_mappings(
		&self,
		_source: &str,
		_env: &GeneralContext,
		_types: &TypeStore,
	) -> String {
		todo!()
		// let mut buf = "Expression type mappings:\n".to_owned();
		// for (pos, instance) in self.expressions_to_instances.iter() {
		// 	buf.push_str(
		// 		source.get((pos.0.start as usize)..(pos.0.end as usize)).unwrap_or_default(),
		// 	);
		// 	buf.push_str(" has type ");
		// 	buf.push_str(&print_type(types, instance.get_value(), env));
		// 	buf.push('\n')
		// }
		// buf
	}
}

/// See <https://www.internalpointers.com/post/understanding-meaning-lexpressions-and-rexpressions-c> for a understanding
/// of `LValue` vs `RValue`
#[derive(Clone, Debug)]
pub enum Instance {
	LValue(VariableWithValue),
	RValue(TypeId),
	/// Result FROM getter
	GValue(TypeId),
}

impl Instance {
	#[must_use]
	pub fn get_variable_id(&self) -> Option<VariableId> {
		match self {
			Self::LValue(variable) => Some(variable.0.get_id()),
			Self::RValue(_) | Self::GValue(_) => None,
		}
	}

	#[must_use]
	pub fn get_value(self) -> TypeId {
		match self {
			Instance::LValue(l) => l.1,
			Instance::GValue(value) | Instance::RValue(value) => value,
		}
	}

	#[must_use]
	pub fn get_value_on_ref(&self) -> TypeId {
		match self {
			Instance::LValue(l) => l.1,
			Instance::GValue(value) | Instance::RValue(value) => *value,
		}
	}
}

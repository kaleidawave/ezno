use std::collections::HashMap;

use crate::{context::VariableId, FunctionPointer, TypeId};

/// Different *components* for JSX elements
/// e.g `<*component_name*>`
#[derive(Debug, Clone)]
pub enum JSXComponentReference {
	Function(Vec<FunctionPointer>),
	Class(VariableId),
	// ViaTagName(TagNamedMapping),
}

// /// TODO not sure might be TypeId
// #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
// pub enum TagNamedMapping {
// 	CustomElement(TypeId),
// 	Inbuilt(TypeId),
// }

struct Things {
	elements_values: HashMap<TypeId, Element>,
	reverse: HashMap<TypeId, ForwardInterpolationSpot>,
}

struct Element {
	// TODO also doubles as properties
	attributes: HashMap<TypeId, TypeId>,
	children: Vec<TypeId>,
}

pub enum ForwardInterpolationSpot {
	Attribute { name: TypeId },
	NodeChild { idx: usize },
}

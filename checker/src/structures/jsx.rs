use crate::TypeId;

pub enum ForwardInterpolationSpot {
	Attribute { name: TypeId },
	NodeChild { idx: usize },
}

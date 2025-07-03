pub use crate::{
	subtyping::{type_is_subtype_object, SubTypeResult},
	CheckingData, RootContext, TypeCheckOptions, TypeId,
};

pub struct Space {
	root: RootContext,
	checking_data: CheckingData<'static, EmptyReciever, crate::synthesis::EznoParser>,
}

struct EmptyReciever;

impl crate::ReadFromFS for EmptyReciever {
	fn read_file(&self, _path: &std::path::Path) -> Option<Vec<u8>> {
		None
	}
}

impl Space {
	pub fn new() -> Self {
		let root = RootContext::new_with_primitive_references();
		let checking_data =
			CheckingData::new(TypeCheckOptions::default(), &EmptyReciever, None, ());
		Self { root, checking_data }
	}

	// Basic boolean version
	pub fn is_assignable_to(&mut self, ty1: &str, ty2: &str) -> bool {
		use parser::ASTNode;

		let mut environment = self.root.new_testing_context();

		let ty1 = parser::TypeAnnotation::from_string(ty1.to_owned(), Default::default()).unwrap();
		let ty2 = parser::TypeAnnotation::from_string(ty2.to_owned(), Default::default()).unwrap();

		let ty1 = crate::synthesis::type_annotations::synthesise_type_annotation(
			&ty1,
			&mut environment,
			&mut self.checking_data,
		);
		let ty2 = crate::synthesis::type_annotations::synthesise_type_annotation(
			&ty2,
			&mut environment,
			&mut self.checking_data,
		);

		let result =
			type_is_subtype_object(ty2, ty1, &mut environment, &mut self.checking_data.types);

		matches!(result, SubTypeResult::IsSubType)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn assignable() {
		let mut space = Space::new();

		assert!(space.is_assignable_to("number", "number | string"));
		assert!(!space.is_assignable_to("number | string", "number"));
	}
}

use super::{GenericArguments, SliceArguments, TypeId};
use crate::features::objects::Proxy;

/// Wraps logic
#[derive(Debug, Clone)]
pub enum Logical<T> {
	Pure(T),
	/// Note this uses [`PossibleLogical<T>`] rather than [`Logical<T>`].
	Or {
		/// This can be [`TypeId::BOOLEAN_TYPE`] for unknown left-right-ness
		condition: TypeId,
		left: Box<LogicalOrValid<T>>,
		right: Box<LogicalOrValid<T>>,
	},
	/// Passes down [`GenericArguments`] found trying to get to source
	Implies {
		on: Box<Self>,
		antecedent: GenericArguments,
	},
	/// WIP mainly for mapped type properties
	BasedOnKey(BasedOnKey<T>),
}

#[derive(Debug, Clone)]
pub enum BasedOnKey<T> {
	Left { value: Box<Logical<T>>, key_arguments: SliceArguments },
	Right(PropertyOn),
}

/// TODO where does Error type fit in here?
#[derive(Debug, Clone)]
pub enum LogicalOrValid<T> {
	Logical(Logical<T>),
	NeedsCalculation(NeedsCalculation),
}

impl<T> From<Logical<T>> for LogicalOrValid<T> {
	fn from(l: Logical<T>) -> LogicalOrValid<T> {
		LogicalOrValid::Logical(l)
	}
}

impl<T> From<NeedsCalculation> for LogicalOrValid<T> {
	fn from(l: NeedsCalculation) -> LogicalOrValid<T> {
		LogicalOrValid::NeedsCalculation(l)
	}
}

// /// TODO split up
// #[derive(Debug, Clone)]
// pub enum MissingOrToCalculate {
// 	/// Doesn't contain request
// 	Missing,
// 	/// From [`TypeId::ERROR_TYPE`]
// 	Error,
// 	/// From [`TypeId::ANY_TYPE`]
// 	Infer { on: TypeId },
// 	/// Proxies require extra work in some cases
// 	Proxy(Proxy),
// }

#[derive(Debug, Clone)]
pub enum NeedsCalculation {
	/// From [`TypeId::ANY_TYPE`]
	Infer { on: TypeId },
	/// Proxies require extra work in some cases. `TypeId` points to proxy
	Proxy(Proxy, TypeId),
}

#[derive(Debug, Clone, Copy)]
pub struct Invalid(pub TypeId);

impl Invalid {
	fn _is_null(self) -> bool {
		self.0 == TypeId::NULL_TYPE
	}
}

/// TODO explain
pub type PossibleLogical<T> = Result<LogicalOrValid<T>, Invalid>;
// pub type PossibleLogical2<T> = Result<Logical<T>, ()>;

#[derive(Debug, Clone)]
pub struct PropertyOn {
	pub on: TypeId,
	pub key: TypeId,
}

impl PropertyOn {
	pub fn get_on(
		self,
		generics: crate::types::GenericChain,
		info: &impl crate::context::InformationChain,
		types: &mut crate::TypeStore,
	) -> Option<TypeId> {
		use crate::types::{get_constraint, properties, PartiallyAppliedGenerics, Type};
		let filter = get_constraint(self.key, types).unwrap_or(self.key);

		crate::utilities::notify!("filter={:?}", types.get_type_by_id(filter));

		let entries = properties::list::get_properties_on_single_type2(
			(self.on, generics),
			types,
			info,
			filter,
		);

		let mut iter = entries.into_iter();
		if let Some((_, first_value, _)) = iter.next() {
			// TODO should properly evaluate value
			let mut value = first_value.as_get_type(types);
			for (_, other, _) in iter {
				value = types.new_or_type(value, other.as_get_type(types));
			}

			let complete = !matches!(
				types.get_type_by_id(filter),
				Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: TypeId::CASE_INSENSITIVE,
					arguments: _,
				})
			);
			if complete {
				value = types.new_or_type(value, TypeId::UNDEFINED_TYPE);
			}

			Some(value)
		} else {
			None
		}
	}
}

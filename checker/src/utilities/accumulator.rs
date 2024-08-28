/// WIP
/// In the context of the type checker
#[must_use]
#[derive(Default, Debug)]
pub enum Accumulator<C, V> {
	Some(V),
	Accumulating {
		condition: C,
		value: V,
	},
	#[default]
	None,
}

impl<C, V> From<Option<V>> for Accumulator<C, V> {
	fn from(on: Option<V>) -> Self {
		match on {
			Some(v) => Self::Some(v),
			None => Self::None,
		}
	}
}

pub trait Condition: Clone + Copy {
	type Associate;

	/// !self
	fn invert(self, helper: &mut Self::Associate) -> Self;

	/// self ∧ other
	fn and(self, other: Self, helper: &mut Self::Associate) -> Self;

	/// self ∨ other
	fn or(self, other: Self, helper: &mut Self::Associate) -> Self;

	/// if self ? left : right
	fn condition(self, left: Self, right: Self, helper: &mut Self::Associate) -> Self;
}

pub trait Result<C: Condition> {
	/// if condition ? left : right.
	// TODO bad definition to re use thing
	fn new_condition(
		condition: C,
		left: Self,
		right: Self,
		helper: &mut <C as Condition>::Associate,
	) -> Self;
}

impl<C, V> Accumulator<C, V> {
	pub fn is_finished(&self) -> bool {
		matches!(self, Self::Some(_))
	}
}

impl<C: Condition, V: Result<C>> Accumulator<C, V> {
	pub fn append(&mut self, new: V, helper: &mut C::Associate) {
		match self {
			Self::Some(_) => {
				crate::utilities::notify!("unreachable");
			}
			Self::Accumulating { .. } => {
				let Self::Accumulating { condition, value: existing } = std::mem::take(self) else {
					unreachable!();
				};
				*self = Self::Some(V::new_condition(condition, existing, new, helper));
			}
			Self::None => {
				*self = Self::Some(new);
			}
		}
	}

	pub fn merge(self, other: Self, condition: C, helper: &mut C::Associate) -> Self {
		match (self, other) {
			(Self::Some(l), Self::Some(r)) => Self::Some(V::new_condition(condition, l, r, helper)),
			(Self::Some(l), Self::Accumulating { condition: r_condition, value: r }) => {
				Self::Accumulating {
					condition: condition.or(r_condition, helper),
					value: V::new_condition(r_condition, l, r, helper),
				}
			}
			(Self::Some(value), Self::None) => Self::Accumulating { condition, value },
			(Self::Accumulating { condition: l_condition, value: l }, Self::Some(r)) => {
				Self::Accumulating {
					condition: l_condition.or(condition, helper),
					value: V::new_condition(l_condition, l, r, helper),
				}
			}
			(
				Self::Accumulating { condition: l_condition, value: l },
				Self::Accumulating { condition: r_condition, value: r },
			) => Self::Accumulating {
				condition: condition.condition(l_condition, r_condition, helper),
				value: V::new_condition(condition, l, r, helper),
			},
			(Self::Accumulating { condition: l_condition, value: l }, Self::None) => {
				Self::Accumulating { condition: condition.and(l_condition, helper), value: l }
			}
			(Self::None, Self::Some(value)) => {
				Self::Accumulating { condition: condition.invert(helper), value }
			}
			(Self::None, Self::Accumulating { condition: r_condition, value }) => {
				Self::Accumulating {
					condition: condition.invert(helper).and(r_condition, helper),
					value,
				}
			}
			(Self::None, Self::None) => Self::None,
		}
	}
}

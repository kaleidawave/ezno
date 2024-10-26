type BetterF64 = ordered_float::NotNan<f64>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InclusiveExclusive {
	Inclusive,
	Exclusive,
}

use InclusiveExclusive::{Exclusive, Inclusive};

impl InclusiveExclusive {
	#[must_use]
	pub fn mix(self, other: Self) -> Self {
		if let (Inclusive, Inclusive) = (self, other) {
			Inclusive
		} else {
			Exclusive
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FloatRange {
	pub floor: (InclusiveExclusive, BetterF64),
	pub ceiling: (InclusiveExclusive, BetterF64),
}

impl Default for FloatRange {
	fn default() -> Self {
		Self {
			floor: (Exclusive, f64::MIN.try_into().unwrap()),
			ceiling: (Exclusive, f64::MAX.try_into().unwrap()),
		}
	}
}

// TODO try_from (assert ceiling > floor etc)
impl FloatRange {
	#[must_use]
	pub fn single(on: BetterF64) -> Self {
		Self { floor: (Inclusive, on), ceiling: (Inclusive, on) }
	}

	pub fn as_single(self) -> Option<BetterF64> {
		if let FloatRange { floor: (Inclusive, floor), ceiling: (Inclusive, ceiling) } = self {
			(floor == ceiling).then_some(floor)
		} else {
			None
		}
	}

	/// For disjointness. TODO Think this is correct
	#[must_use]
	pub fn overlaps(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} ∩ {:?} != ∅", self, other);

		// TODO more with inclusivity etc
		other.floor.1 <= self.ceiling.1 || other.ceiling.1 <= self.floor.1
	}

	pub fn intersection(self, other: Self) -> Result<Self, ()> {
		crate::utilities::notify!("{:?} ∩ {:?}", self, other);

		let max_floor = self.floor.1.max(other.floor.1);
		let min_ceiling = self.ceiling.1.min(other.ceiling.1);

		if max_floor <= min_ceiling {
			Ok(Self {
				floor: (self.floor.0.mix(other.floor.0), max_floor),
				ceiling: (self.ceiling.0.mix(other.ceiling.0), min_ceiling),
			})
		} else {
			Err(())
		}
	}

	/// The ⊆ relation (non-strict). For subtyping. TODO Think this is correct
	#[must_use]
	pub fn contained_in(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} ⊆ {:?}", self, other);
		// Edge case
		let lhs = if let (Inclusive, Exclusive) = (self.floor.0, other.floor.0) {
			self.floor.1 > other.floor.1
		} else {
			self.floor.1 >= other.floor.1
		};
		let rhs = if let (Inclusive, Exclusive) = (self.ceiling.0, other.ceiling.0) {
			self.ceiling.1 < other.ceiling.1
		} else {
			self.ceiling.1 <= other.ceiling.1
		};
		lhs && rhs
	}

	/// ∀ a in self, ∀ b in other: a > b
	#[must_use]
	pub fn above(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} > {:?}", self, other);
		if let (Inclusive, Inclusive) = (self.ceiling.0, other.floor.0) {
			self.floor.1 > other.ceiling.1
		} else {
			self.floor.1 >= other.ceiling.1
		}
	}

	/// ∀ a in self, ∀ b in other: a < b
	#[must_use]
	pub fn below(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} < {:?}", self, other);
		if let (Inclusive, Inclusive) = (self.ceiling.0, other.floor.0) {
			self.ceiling.1 < other.floor.1
		} else {
			self.ceiling.1 <= other.floor.1
		}
	}

	#[must_use]
	pub fn space_addition(self, other: Self) -> Self {
		let floor_bound = self.floor.0.mix(other.floor.0);
		let ceiling_bound = self.ceiling.0.mix(other.ceiling.0);
		Self {
			floor: (floor_bound, self.floor.1 + other.floor.1),
			ceiling: (ceiling_bound, self.ceiling.1 + other.ceiling.1),
		}
	}

	#[must_use]
	pub fn space_multiplication(self, other: Self) -> Self {
		let (l_floor, l_ceiling, r_floor, r_ceiling) =
			(self.floor.1, self.ceiling.1, other.floor.1, other.ceiling.1);
		// there may be a faster way but being lazy
		let corners =
			[l_floor * r_floor, l_floor * r_ceiling, r_floor * l_ceiling, l_ceiling * r_ceiling];
		let floor = *corners.iter().min().unwrap();
		let ceiling = *corners.iter().max().unwrap();

		let floor_bound = self.floor.0.mix(other.floor.0);
		let ceiling_bound = self.ceiling.0.mix(other.ceiling.0);
		Self { floor: (floor_bound, floor), ceiling: (ceiling_bound, ceiling) }
	}

	#[must_use]
	pub fn contains_multiple_of(self, multiple_of: BetterF64) -> bool {
		let (floor, ceiling) = (self.floor.1, self.ceiling.1);

		let floor = floor / multiple_of;
		let ceiling = ceiling / multiple_of;

		// TODO >= ?
		ceiling.floor() > *floor
	}

	// TODO double check
	// TODO what happens when disjoint
	#[must_use]
	pub fn intersection(self, other: Self) -> Self {
		let floor = if self.floor.0 < other.floor.0 {
			other.floor
		} else {
			self.floor
		};
		let ceiling = if self.ceiling.0 < other.ceiling.0 {
			self.ceiling
		} else {
			other.ceiling
		};
		Self {
			floor,
			ceiling,
		}
	}

	// This will try to get cover
	// A union like above might create gaps. aka if try_get_cover (0, 1) (3, 4) = (0, 4) then it implies 2 
	// exists is in one of the ranges. Thus in this case it returns None
	pub fn try_get_cover(self, other: Self) -> Option<Self> {
		if self.contained_in(other) {
			Some(other)
		} else if other.contained_in(self) {
			Some(self)
		} else {
			None
		}
	}

	// TODO more :)
}

impl From<std::ops::Range<BetterF64>> for FloatRange {
	fn from(range: std::ops::Range<BetterF64>) -> FloatRange {
		FloatRange { floor: (Exclusive, range.start), ceiling: (Exclusive, range.end) }
	}
}
impl TryFrom<std::ops::Range<f64>> for FloatRange {
	type Error = ordered_float::FloatIsNan;

	fn try_from(range: std::ops::Range<f64>) -> Result<Self, Self::Error> {
		let floor = ordered_float::NotNan::new(range.start)?;
		let ceiling = ordered_float::NotNan::new(range.end)?;
		Ok(FloatRange { floor: (Exclusive, floor), ceiling: (Exclusive, ceiling) })
	}
}

// TODO more
#[cfg(test)]
mod tests {
	use super::{BetterF64, FloatRange, InclusiveExclusive};

	fn e(a: f64) -> (InclusiveExclusive, BetterF64) {
		(InclusiveExclusive::Exclusive, a.try_into().unwrap())
	}

	fn i(a: f64) -> (InclusiveExclusive, BetterF64) {
		(InclusiveExclusive::Inclusive, a.try_into().unwrap())
	}

	#[test]
	fn contained_in() {
		let zero_to_four: FloatRange = FloatRange::try_from(0f64..4f64).unwrap();
		assert!(FloatRange::single(2.into()).contained_in(zero_to_four));
	}

	#[test]
	fn overlaps() {
		assert!(FloatRange { floor: e(0.), ceiling: e(4.) }
			.overlaps(FloatRange { floor: e(2.), ceiling: e(5.) }));

		assert!(!FloatRange { floor: e(0.), ceiling: e(1.) }
			.overlaps(FloatRange { floor: e(2.), ceiling: e(5.) }));
	}

	#[test]
	fn above() {
		assert!(FloatRange { floor: e(8.), ceiling: e(10.) }
			.above(FloatRange { floor: e(6.), ceiling: e(7.) }));
		assert!(!FloatRange { floor: e(0.), ceiling: e(1.) }
			.above(FloatRange { floor: e(0.), ceiling: e(5.) }));
	}

	#[test]
	fn below() {
		assert!(FloatRange { floor: e(0.), ceiling: e(4.) }
			.below(FloatRange { floor: e(6.), ceiling: e(7.) }));
		assert!(!FloatRange { floor: e(0.), ceiling: e(1.) }
			.below(FloatRange { floor: e(0.), ceiling: e(5.) }));
	}

	#[test]
	fn space_addition() {
		let lhs = FloatRange { floor: e(0.), ceiling: e(4.) }
			.space_addition(FloatRange { floor: e(6.), ceiling: e(7.) });
		assert_eq!(lhs, FloatRange { floor: e(6.), ceiling: e(11.) });
	}

	#[test]
	fn space_multiplication() {
		let lhs = FloatRange { floor: e(0.), ceiling: e(4.) }
			.space_multiplication(FloatRange { floor: e(6.), ceiling: e(7.) });
		assert_eq!(lhs, FloatRange { floor: e(0.), ceiling: e(28.) });

		let lhs = FloatRange { floor: e(-2.), ceiling: e(4.) }
			.space_multiplication(FloatRange { floor: e(-10.), ceiling: e(1.) });
		assert_eq!(lhs, FloatRange { floor: e(-40.), ceiling: e(20.) });
	}

	#[test]
	fn multiple_of() {
		let lhs = FloatRange { floor: e(30.), ceiling: e(34.) };
		assert!(lhs.contains_multiple_of(4.into())); // 32
		assert!(!lhs.contains_multiple_of(7.into())); // 28 -- 35
	}
}

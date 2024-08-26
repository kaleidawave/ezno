type BetterF64 = ordered_float::NotNan<f64>;

// TODO
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatRange {
	/// yes or `===`
	Inclusive { floor: BetterF64, ceiling: BetterF64 },
	/// but not necessarily `===`
	Exclusive { floor: BetterF64, ceiling: BetterF64 },
}

impl FloatRange {
	#[must_use]
	pub fn single(on: BetterF64) -> Self {
		Self::Inclusive { floor: on, ceiling: on }
	}

	/// For disjointness. TODO Think this is correct
	#[must_use]
	pub fn overlaps(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} ∩ {:?} != ∅", self, other);

		if let (
			Self::Inclusive { floor: l_floor, ceiling: l_ceiling },
			Self::Inclusive { floor: r_floor, ceiling: r_ceiling },
		) = (self, other)
		{
			if l_floor <= r_floor {
				l_ceiling >= r_floor
			} else if l_ceiling >= r_ceiling {
				l_floor <= r_ceiling
			} else {
				false
			}
		} else {
			let (Self::Inclusive { floor: l_floor, ceiling: l_ceiling }
			| Self::Exclusive { floor: l_floor, ceiling: l_ceiling }) = self;
			let (Self::Inclusive { floor: r_floor, ceiling: r_ceiling }
			| Self::Exclusive { floor: r_floor, ceiling: r_ceiling }) = other;
			if l_floor < r_floor {
				l_ceiling > r_floor
			} else if l_ceiling > r_ceiling {
				l_floor < r_ceiling
			} else {
				false
			}
		}
	}

	/// The ⊆ relation (non-strict). For subtyping. TODO Think this is correct
	#[must_use]
	pub fn contained_in(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} ⊆ {:?}", self, other);
		// Edge case
		if let (
			Self::Inclusive { floor: l_floor, ceiling: l_ceiling },
			Self::Exclusive { floor: r_floor, ceiling: r_ceiling },
		) = (self, other)
		{
			l_floor > r_floor && l_ceiling < r_ceiling
		} else {
			let (Self::Inclusive { floor: l_floor, ceiling: l_ceiling }
			| Self::Exclusive { floor: l_floor, ceiling: l_ceiling }) = self;
			let (Self::Inclusive { floor: r_floor, ceiling: r_ceiling }
			| Self::Exclusive { floor: r_floor, ceiling: r_ceiling }) = other;
			l_floor >= r_floor && l_ceiling <= r_ceiling
		}
	}

	/// ∀ a in self, ∀ b in other: a > b
	#[must_use]
	pub fn above(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} > {:?}", self, other);
		if let (
			Self::Inclusive { floor: l_floor, ceiling: _ },
			Self::Inclusive { floor: _, ceiling: r_ceiling },
		) = (self, other)
		{
			l_floor > r_ceiling
		} else {
			let (Self::Inclusive { floor: l_floor, ceiling: _ }
			| Self::Exclusive { floor: l_floor, ceiling: _ }) = self;
			let (Self::Inclusive { floor: _, ceiling: r_ceiling }
			| Self::Exclusive { floor: _, ceiling: r_ceiling }) = other;
			l_floor >= r_ceiling
		}
	}

	/// ∀ a in self, ∀ b in other: a < b
	#[must_use]
	pub fn below(self, other: Self) -> bool {
		crate::utilities::notify!("{:?} < {:?}", self, other);
		if let (
			Self::Inclusive { floor: _, ceiling: l_ceiling },
			Self::Inclusive { floor: r_floor, ceiling: _ },
		) = (self, other)
		{
			l_ceiling < r_floor
		} else {
			let (Self::Inclusive { floor: _, ceiling: l_ceiling }
			| Self::Exclusive { floor: _, ceiling: l_ceiling }) = self;
			let (Self::Inclusive { floor: r_floor, ceiling: _ }
			| Self::Exclusive { floor: r_floor, ceiling: _ }) = other;
			l_ceiling <= r_floor
		}
	}

	#[must_use]
	pub fn space_addition(self, other: Self) -> Self {
		if let (
			Self::Inclusive { floor: l_floor, ceiling: l_ceiling },
			Self::Inclusive { floor: r_floor, ceiling: r_ceiling },
		) = (self, other)
		{
			Self::Inclusive { floor: l_floor + r_floor, ceiling: l_ceiling + r_ceiling }
		} else {
			let (Self::Inclusive { floor: l_floor, ceiling: l_ceiling }
			| Self::Exclusive { floor: l_floor, ceiling: l_ceiling }) = self;
			let (Self::Inclusive { floor: r_floor, ceiling: r_ceiling }
			| Self::Exclusive { floor: r_floor, ceiling: r_ceiling }) = other;
			Self::Exclusive { floor: l_floor + r_floor, ceiling: l_ceiling + r_ceiling }
		}
	}

	#[must_use]
	pub fn space_multiplication(self, other: Self) -> Self {
		let inclusive = matches!((self, other), (Self::Inclusive { .. }, Self::Inclusive { .. }));
		let (Self::Inclusive { floor: l_floor, ceiling: l_ceiling }
		| Self::Exclusive { floor: l_floor, ceiling: l_ceiling }) = self;
		let (Self::Inclusive { floor: r_floor, ceiling: r_ceiling }
		| Self::Exclusive { floor: r_floor, ceiling: r_ceiling }) = other;
		// being lazy
		let corners =
			[l_floor * r_floor, l_floor * r_ceiling, r_floor * l_ceiling, l_ceiling * r_ceiling];
		let floor = *corners.iter().min().unwrap();
		let ceiling = *corners.iter().max().unwrap();
		if inclusive {
			Self::Inclusive { floor, ceiling }
		} else {
			Self::Exclusive { floor, ceiling }
		}
	}

	// TODO more :)
}

// TODO more
#[cfg(test)]
mod tests {
	use super::{BetterF64, FloatRange};

	#[test]
	fn contained_in() {
		assert!(FloatRange::single(2.into())
			.contained_in(FloatRange::Exclusive { floor: 0.into(), ceiling: 5.into() }));
	}

	#[test]
	fn overlaps() {
		assert!(FloatRange::Exclusive { floor: 0.into(), ceiling: 4.into() }
			.overlaps(FloatRange::Exclusive { floor: 2.into(), ceiling: 5.into() }));
		assert!(!FloatRange::Exclusive { floor: 0.into(), ceiling: 1.into() }
			.overlaps(FloatRange::Exclusive { floor: 2.into(), ceiling: 5.into() }));
	}

	#[test]
	fn above() {
		assert!(FloatRange::Exclusive { floor: 8.into(), ceiling: 10.into() }
			.above(FloatRange::Exclusive { floor: 6.into(), ceiling: 7.into() }));
		assert!(!FloatRange::Exclusive { floor: 0.into(), ceiling: 1.into() }
			.above(FloatRange::Exclusive { floor: 0.into(), ceiling: 5.into() }));
	}

	#[test]
	fn below() {
		assert!(FloatRange::Exclusive { floor: 0.into(), ceiling: 4.into() }
			.below(FloatRange::Exclusive { floor: 6.into(), ceiling: 7.into() }));
		assert!(!FloatRange::Exclusive { floor: 0.into(), ceiling: 1.into() }
			.below(FloatRange::Exclusive { floor: 0.into(), ceiling: 5.into() }));
	}

	#[test]
	fn space_addition() {
		assert_eq!(
			FloatRange::Exclusive { floor: 0.into(), ceiling: 4.into() }
				.space_addition(FloatRange::Exclusive { floor: 6.into(), ceiling: 7.into() }),
			FloatRange::Exclusive { floor: 6.into(), ceiling: 11.into() }
		);
	}

	#[test]
	fn space_multiplication() {
		assert_eq!(
			FloatRange::Exclusive { floor: 0.into(), ceiling: 4.into() }
				.space_multiplication(FloatRange::Exclusive { floor: 6.into(), ceiling: 7.into() }),
			FloatRange::Exclusive { floor: 0.into(), ceiling: 28.into() }
		);
		assert_eq!(
			FloatRange::Exclusive { floor: BetterF64::from(-2i32), ceiling: 4.into() }
				.space_multiplication(FloatRange::Exclusive {
					floor: BetterF64::from(-10i32),
					ceiling: 1.into()
				}),
			FloatRange::Exclusive { floor: BetterF64::from(-40i32), ceiling: 20.into() }
		);
	}
}

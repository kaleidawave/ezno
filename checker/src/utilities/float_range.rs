// TODO
#[derive(Debug, Clone, Copy)]
pub enum FloatRange {
	/// yes or `===`
	Inclusive { floor: ordered_float::NotNan<f64>, ceiling: ordered_float::NotNan<f64> },
	/// but not necessarily `===`
	Exclusive { floor: ordered_float::NotNan<f64>, ceiling: ordered_float::NotNan<f64> },
}

impl FloatRange {
	#[must_use]
	pub fn single(on: ordered_float::NotNan<f64>) -> Self {
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
}

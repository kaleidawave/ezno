type BetterF64 = ordered_float::NotNan<f64>;

/// x ≡ *class* [mod *modulo*]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ModuloClass {
	pub modulo: BetterF64,
	pub offset: BetterF64,
}

// TODO more operations
impl ModuloClass {
	pub fn new(modulo: BetterF64, offset: BetterF64) -> Self {
		debug_assert!(modulo != 0.);
		if modulo > 0f64.try_into().unwrap() {
			Self { offset: offset % modulo, modulo }
		} else {
			// TODO think this is correct. [1]_{-3} = [2]_{3}
			let modulo = -modulo;
			Self { offset: modulo - (offset % modulo), modulo }
		}
	}

	pub fn contains(self, value: BetterF64) -> bool {
		// Note -0. = 0.
		(value - self.offset) % self.modulo == 0.
	}

	/// WIP
	pub fn disjoint(self, other: Self) -> bool {
		if let Ok(gcd) = gcd_of_float(self.modulo, other.modulo) {
			crate::utilities::notify!("{:?}", gcd);
			(self.offset - other.offset) % gcd != 0.
		} else {
			crate::utilities::notify!("Here");
			true
		}
	}

	pub fn intersection(self, _other: Self) -> Option<Self> {
		todo!()
	}

	pub fn get_cover(self, _other: Self) -> Option<Self> {
		todo!()
	}

	pub fn offset(self, offset: BetterF64) -> Self {
		// TODO temp fix
		if self.is_default() {
			self
		} else {
			Self::new(self.modulo, self.offset + offset)
		}
	}

	pub fn multiply(self, multiple: BetterF64) -> Self {
		// TODO temp fix
		if self.is_default() {
			self
		} else {
			Self::new(self.modulo * multiple, self.offset)
		}
	}

	pub fn negate(self) -> Self {
		// TODO temp fix
		if self.is_default() {
			self
		} else {
			Self::new(self.modulo, self.modulo - self.offset)
		}
	}

	pub fn is_default(self) -> bool {
		self.modulo == f64::EPSILON
	}
}

/// Using Farey algoirthm
/// TODO is there a faster way implemntation
/// Note that numerator and denominator are coprime
fn try_get_numerator_denominator(input: BetterF64) -> Result<(i32, i32), ()> {
	const STEPS: usize = 50;
	const MARGIN: f64 = 1e-4;

	let integer_part = input.trunc() as i32;
	let fractional_part = input.fract();
	if fractional_part == 0. {
		return Ok((integer_part, 1));
	}

	let (mut a, mut b, mut c, mut d) = (0, 1, 1, 1);

	for _ in 0..STEPS {
		let mediant_float = (a as f64 + b as f64) / (c as f64 + d as f64);
		if (fractional_part - mediant_float).abs() < MARGIN {
			let numerator = a + b + integer_part * (c + d);
			let denominator = c + d;
			return Ok((numerator, denominator));
		} else if fractional_part > mediant_float {
			a = a + b;
			c = c + d;
		} else {
			b = a + b;
			d = c + d;
		}
	}

	Err(())
}

fn gcd_of_float(n1: BetterF64, n2: BetterF64) -> Result<BetterF64, ()> {
	fn gcd(mut n1: i32, mut n2: i32) -> i32 {
		while n2 != 0 {
			let t = n2;
			n2 = n1 % n2;
			n1 = t;
		}
		n1
	}

	/// n1*n2 = gcd(n1, n2)*lcm(n1, n2)
	fn lcm(n1: i32, n2: i32) -> i32 {
		(n1 * n2) / gcd(n1, n2)
	}

	let (a, b) = try_get_numerator_denominator(n1)?;
	let (c, d) = try_get_numerator_denominator(n2)?;

	// gcd(a / b, c / d) = gcd(a, c) / lcm(b, d)
	Ok(BetterF64::new(gcd(a, c) as f64 / lcm(b, d) as f64).unwrap())
}

// hmmm
impl Default for ModuloClass {
	fn default() -> Self {
		Self { modulo: f64::EPSILON.try_into().unwrap(), offset: BetterF64::new(0.).unwrap() }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	// TODO test negatives etc
	#[test]
	fn gcd() {
		assert_eq!(gcd_of_float(1. / 3., 3. / 2.), Ok(3.));
	}
}

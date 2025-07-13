use super::derive_ASTNode;
use std::{borrow::Cow, str::FromStr};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[apply(derive_ASTNode)]
pub enum NumberSign {
	/// Also implies non negative/missing
	Positive,
	Negative,
}

impl NumberSign {
	pub fn apply<T: std::ops::Neg<Output = T>>(&self, x: T) -> T {
		match self {
			NumberSign::Positive => x,
			NumberSign::Negative => -x,
		}
	}
}

impl std::ops::Neg for NumberSign {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			NumberSign::Positive => NumberSign::Negative,
			NumberSign::Negative => NumberSign::Positive,
		}
	}
}

impl std::fmt::Display for NumberSign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if matches!(self, Self::Negative) {
			f.write_str("-")
		} else {
			Ok(())
		}
	}
}

/// Some of these can't be parsed into this form, but are here so that
/// a number representation can be generated from a f64
///
/// <https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-literals-numeric-literals>
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum NumberRepresentation {
	Infinity,
	NegativeInfinity,
	NaN,
	Hex { sign: NumberSign, value: u64 },
	Bin { sign: NumberSign, value: u64 },
	Octal { sign: NumberSign, value: u64 },
	Number(f64),
	Exponential { sign: NumberSign, value: f64, exponent: i32 },
	BigInt(NumberSign, String),
}

impl std::hash::Hash for NumberRepresentation {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
	}
}

impl TryFrom<NumberRepresentation> for f64 {
	// BigInt!!
	type Error = ();

	fn try_from(this: NumberRepresentation) -> Result<Self, Self::Error> {
		match this {
			NumberRepresentation::Infinity => Ok(f64::INFINITY),
			NumberRepresentation::NegativeInfinity => Ok(f64::NEG_INFINITY),
			NumberRepresentation::NaN => Ok(f64::NAN),
			NumberRepresentation::Number(value) => Ok(value),
			NumberRepresentation::Hex { sign, value, .. }
			| NumberRepresentation::Bin { sign, value, .. }
			| NumberRepresentation::Octal { sign, value, .. } => {
				// TODO `value as f64` can lose information? If so should return f64::INFINITY
				#[allow(clippy::cast_precision_loss)]
				Ok(sign.apply(value as f64))
			}
			NumberRepresentation::Exponential { sign, value, exponent } => {
				Ok(sign.apply(value * 10f64.powi(exponent)))
			}
			NumberRepresentation::BigInt(..) => Err(()),
		}
	}
}

// For code generation
impl From<f64> for NumberRepresentation {
	fn from(value: f64) -> Self {
		if value == f64::INFINITY {
			Self::Infinity
		} else if value == f64::NEG_INFINITY {
			Self::NegativeInfinity
		} else if value.is_nan() {
			Self::NaN
		} else {
			Self::Number(value)
		}
	}
}

impl FromStr for NumberRepresentation {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if s == "NaN" {
			return Ok(Self::NaN);
		}

		if s.contains('_') {
			return s.replace('_', "").parse();
		}

		let (sign, s) = if let Some(s) = s.strip_prefix('-') {
			(NumberSign::Negative, s)
		} else {
			(NumberSign::Positive, s)
		};

		let s = if s.contains('_') { Cow::Owned(s.replace('_', "")) } else { Cow::Borrowed(s) };

		if let Some(s) = s.strip_suffix('n') {
			Ok(NumberRepresentation::BigInt(sign, s.to_owned()))
		} else if let (Some(s), false) = (s.strip_prefix('0'), s[1..].starts_with('.')) {
			let next_char = s.chars().next();
			match next_char {
				Some('X' | 'x') => {
					let mut value = 0u64;
					for c in &s.as_bytes()[1..] {
						value <<= 4; // 16=2^4
						match c {
							b'0'..=b'9' => {
								value += u64::from(c - b'0');
							}
							b'a'..=b'f' => {
								value += u64::from(c - b'a') + 10;
							}
							b'A'..=b'F' => {
								value += u64::from(c - b'A') + 10;
							}
							_ => return Err(s.to_owned()),
						}
					}
					return Ok(Self::Hex { sign, value });
				}
				Some('B' | 'b') => {
					let mut value = 0u64;
					for c in &s.as_bytes()[1..] {
						value <<= 1;
						match c {
							b'0' | b'1' => {
								value += u64::from(c - b'0');
							}
							_ => return Err(s.to_owned()),
						}
					}
					Ok(Self::Bin { sign, value })
				}
				Some('e' | 'E') => {
					// Lol
					let without_e = &s[1..];
					let exponent: i32 = without_e.parse().map_err(|_| s.to_owned())?;
					Ok(Self::Exponential { sign, value: 0f64, exponent })
				}
				// 'o' | 'O' but can also be missed
				Some(c) => {
					let uses_character = matches!(c, 'o' | 'O');

					if !uses_character && s.contains(['8', '9', '.']) {
						// TODO missed here
						return Ok(Self::Number(sign.apply(s.parse().map_err(|_| s.to_owned())?)));
					}

					// If it uses the the character then skip one, else skip zero
					let start: usize = uses_character.into();

					let mut value = 0u64;
					for c in &s.as_bytes()[start..] {
						value <<= 3; // 8=2^3
						if matches!(c, b'0'..=b'7') {
							value += u64::from(c - b'0');
						} else {
							return Err(s.to_owned());
						}
					}
					Ok(Self::Octal { sign, value })
				}
				None => Ok(Self::Number(0f64)),
			}
		} else if s.starts_with('.') {
			if let "." = s.as_ref() {
				Err("'.' is not a valid number".to_owned())
			} else {
				let value: f64 = format!("0{s}").parse().map_err(|_| s.clone())?;
				Ok(Self::Number(sign.apply(value)))
			}
		} else if let Some(s) = s.strip_suffix('.') {
			Ok(Self::Number(sign.apply(s.parse::<f64>().map_err(|_| s)?)))
		} else if let Some((left, right)) = s.split_once(['e', 'E']) {
			let value = if left.starts_with('.') {
				format!("0{left}").parse::<f64>()
			} else {
				left.parse::<f64>()
			};
			let value = value.map_err(|_| s.clone())?;
			if let Ok(exponent) = right.parse::<i32>() {
				Ok(Self::Exponential { sign, value, exponent })
			} else if right.starts_with('-') || value == 0f64 {
				// lol
				Ok(Self::Number(0f64))
			} else {
				Ok(Self::Infinity)
			}
		} else {
			Ok(Self::Number(sign.apply(s.parse::<f64>().map_err(|_| s.clone())?)))
		}
	}
}

impl std::fmt::Display for NumberRepresentation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.clone().as_js_string())
	}
}

impl std::ops::Neg for NumberRepresentation {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			NumberRepresentation::Infinity => NumberRepresentation::NegativeInfinity,
			NumberRepresentation::NegativeInfinity => NumberRepresentation::Infinity,
			NumberRepresentation::NaN => NumberRepresentation::NaN,
			NumberRepresentation::Hex { sign, value } => {
				NumberRepresentation::Hex { sign: sign.neg(), value }
			}
			NumberRepresentation::Bin { sign, value } => {
				NumberRepresentation::Bin { sign: sign.neg(), value }
			}
			NumberRepresentation::Octal { sign, value } => {
				NumberRepresentation::Octal { sign: sign.neg(), value }
			}
			NumberRepresentation::Number(n) => NumberRepresentation::Number(n.neg()),
			NumberRepresentation::Exponential { sign, value, exponent } => {
				NumberRepresentation::Exponential { sign: sign.neg(), value, exponent }
			}
			NumberRepresentation::BigInt(sign, value) => {
				NumberRepresentation::BigInt(sign.neg(), value)
			}
		}
	}
}

impl NumberRepresentation {
	#[must_use]
	pub fn as_js_string(self) -> String {
		match self {
			NumberRepresentation::Infinity => "Infinity".to_owned(),
			NumberRepresentation::NegativeInfinity => "-Infinity".to_owned(),
			NumberRepresentation::NaN => "NaN".to_owned(),
			NumberRepresentation::Hex { sign, value, .. } => {
				format!("{sign}0x{value:x}")
			}
			NumberRepresentation::Bin { sign, value, .. } => {
				format!("{sign}0b{value:b}")
			}
			NumberRepresentation::Octal { sign, value } => {
				format!("{sign}0o{value:o}")
			}
			NumberRepresentation::Number(value) => value.to_string(),
			NumberRepresentation::Exponential { sign, value, exponent } => {
				format!("{sign}{value}e{exponent}")
			}
			NumberRepresentation::BigInt(s, value) => format!("{s}{value}n"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::NumberRepresentation;
	use std::str::FromStr;

	#[test]
	fn invalid() {
		assert!(NumberRepresentation::from_str(".").is_err());
	}
}

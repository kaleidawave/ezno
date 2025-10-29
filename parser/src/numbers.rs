// --- numbers --

pub enum Context {
	Exponents,
	Hexadecimal,
	Binary,
	Octal,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParsedNumberLiteral<'a> {
	Number(f64),
	/// FUTURE could be some other type
	BigInt(&'a str),
}

#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type NumberRepresentation = f64;

/// Parses a number to a f64 value and returns the width
pub fn parse_number(current: &str) -> Result<(ParsedNumberLiteral<'_>, u32), ()> {
	if let Some(after) = current.strip_prefix('0')
		&& !current[1..].starts_with(['.', 'E', 'e'])
	{
		let specifier = after.chars().next();
		match specifier {
			Some('X' | 'x') => {
				let mut count: u32 = 2;
				let mut value = 0f64;
				// not allowed leading
				let mut seperator_allowed = false;
				for c in &after.as_bytes()[1..] {
					match c {
						b'0'..=b'9' => {
							value *= 16f64;
							value += f64::from(c - b'0');
							seperator_allowed = true;
						}
						b'a'..=b'f' => {
							value *= 16f64;
							value += f64::from(c - b'a') + 10f64;
							seperator_allowed = true;
						}
						b'A'..=b'F' => {
							value *= 16f64;
							value += f64::from(c - b'A') + 10f64;
							seperator_allowed = true;
						}
						b'_' => {
							if seperator_allowed {
								// not allow adjacent
								seperator_allowed = false;
							} else {
								return Err(());
							}
						}
						b'n' => {
							let source = &current[..count as usize];
							return Ok((ParsedNumberLiteral::BigInt(source), count + 1));
						}
						_ => break,
					}
					count += 1;
				}
				// not allowed trailing
				if !seperator_allowed {
					return Err(());
				}
				Ok((ParsedNumberLiteral::Number(value), count))
			}
			Some('B' | 'b') => {
				let mut count: u32 = 2;
				let mut value = 0f64;
				// not allowed leading
				let mut seperator_allowed = false;
				for c in &after.as_bytes()[1..] {
					if let b'0' | b'1' = c {
						value *= 2f64;
						seperator_allowed = true;
						value += f64::from(c - b'0');
					} else if let b'_' = c {
						if seperator_allowed {
							// not allow adjacent
							seperator_allowed = false;
						} else {
							return Err(());
						}
					} else if let b'n' = c {
						let source = &current[..count as usize];
						return Ok((ParsedNumberLiteral::BigInt(source), count + 1));
					} else {
						break;
					}
					count += 1;
				}
				// not allowed trailing
				if !seperator_allowed {
					return Err(());
				}
				Ok((ParsedNumberLiteral::Number(value), count))
			}
			// 'o' | 'O' but can also be missed
			Some(c @ ('o' | 'O' | '.' | '_' | '0'..='9')) => {
				// TODO wip
				fn is_octal_impersonator(rest: &str) -> bool {
					for chr in rest.bytes() {
						if let b'0'..=b'7' = chr {
							continue;
						}

						// hmm `| b'n'`
						let is_not_octal = matches!(chr, b'.' | b'8' | b'9' | b'_');
						return is_not_octal;
					}
					false
				}

				let is_explicit_octal = matches!(c, 'o' | 'O');

				if !is_explicit_octal && is_octal_impersonator(after) {
					let (value, count) = parse_no_specifier(after)?;
					Ok((value, count + 1))
				} else {
					// If it uses the the character then skip one, else skip zero
					let start: usize = is_explicit_octal.into();

					let mut value = 0f64;
					let mut count = 1 + start as u32;
					// not allowed leading
					let mut seperator_allowed = false;
					for c in &after.as_bytes()[start..] {
						if let b'0'..=b'7' = c {
							value *= 8f64;
							seperator_allowed = true;
							value += f64::from(c - b'0');
						} else if let b'_' = c {
							if seperator_allowed {
								// not allow adjacent
								seperator_allowed = false;
							} else {
								return Err(());
							}
						} else if let b'n' = c {
							let source = &current[..count as usize];
							return Ok((ParsedNumberLiteral::BigInt(source), count + 1));
						} else {
							break;
						}
						count += 1;
					}
					// not allowed trailing
					if !seperator_allowed {
						return Err(());
					}
					Ok((ParsedNumberLiteral::Number(value), count))
				}
			}
			Some('n') => Ok((ParsedNumberLiteral::BigInt("0"), 2)),
			Some(_) | None => Ok((ParsedNumberLiteral::Number(0f64), 1)),
		}
	} else {
		parse_no_specifier(current)
	}
}

fn parse_no_specifier(current: &str) -> Result<(ParsedNumberLiteral<'_>, u32), ()> {
	fn number_of_number_characters(current: &str) -> usize {
		let mut after_decimal = false;
		for (idx, byte) in current.bytes().enumerate() {
			if let b'.' = byte {
				if after_decimal {
					return idx;
				}
				after_decimal = true;
			} else {
				let is_valid = byte.is_ascii_digit() || byte == b'_';
				if !is_valid {
					return idx;
				}
			}
		}
		current.len()
	}

	let mut count = number_of_number_characters(current);
	if current[count..].starts_with(['e', 'E']) {
		count += 1;
		if current[count..].starts_with(['-', '+']) {
			count += 1;
		}
		let after = &current[count..];
		count += after.find(|c: char| !c.is_ascii_digit()).unwrap_or(after.len());
	}
	let source = &current[..count];

	if current[count..].starts_with('n') {
		// TODO do we need to check decimals and exponents here
		Ok((ParsedNumberLiteral::BigInt(source), count as u32 + 1))
	} else {
		let new;
		let source = if source.contains('_') {
			if let Ok(value) = try_remove_seperator(source) {
				new = value;
				&new
			} else {
				return Err(());
			}
		} else {
			source
		};

		if let Ok(value) = source.parse::<f64>() {
			Ok((ParsedNumberLiteral::Number(value), count as u32))
		} else {
			Err(())
		}
	}
}

fn try_remove_seperator(on: &str) -> Result<String, ()> {
	let mut buf = String::new();
	let mut last = 0;
	for (idx, _) in on.match_indices('_') {
		let (before, after) = on.split_at(idx);
		let is_invalid = idx == 0 || before.ends_with(['.', '_']) || after.starts_with('.');
		if is_invalid {
			return Err(());
		}
		buf.push_str(&before[last..]);
		last = idx + 1;
	}
	let rest = &on[last..];
	if rest.ends_with('_') {
		Err(())
	} else {
		buf.push_str(rest);
		Ok(buf)
	}
}

#[cfg(test)]
mod tests {
	use super::parse_number;
	use super::{ParsedNumberLiteral::BigInt as n, ParsedNumberLiteral::Number as f};

	#[test]
	fn numbers() {
		assert_eq!(parse_number("42"), Ok((f(42f64), 2)));
		assert_eq!(parse_number("42_000"), Ok((f(42000f64), 6)));
		assert_eq!(parse_number("42.2"), Ok((f(42.2f64), 4)));
		assert_eq!(parse_number(".2"), Ok((f(0.2f64), 2)));
		assert_eq!(parse_number("42."), Ok((f(42f64), 3)));
		assert_eq!(parse_number("1_000.002"), Ok((f(1000.002f64), 9)));
		assert_eq!(parse_number("1_000_000"), Ok((f(1000000f64), 9)));
		assert_eq!(parse_number("0"), Ok((f(0f64), 1)));
		assert_eq!(parse_number("0;"), Ok((f(0f64), 1)));
		assert_eq!(parse_number("0..toString()"), Ok((f(0f64), 2)));
		assert_eq!(parse_number("1..toString()"), Ok((f(1f64), 2)));
		assert_eq!(parse_number("10.2._5"), Ok((f(10.2f64), 4)));
	}

	#[test]
	fn exponents() {
		assert_eq!(parse_number("4.2e3"), Ok((f(4200f64), 5)));
		assert_eq!(parse_number("4.2e+3"), Ok((f(4200f64), 6)));
		assert_eq!(parse_number("42e-4"), Ok((f(0.0042f64), 5)));

		assert_eq!(parse_number("4.2e+500"), Ok((f(f64::INFINITY), 8)));
		assert_eq!(parse_number("4.2e-4000"), Ok((f(0f64), 9)));
	}

	#[test]
	fn hex() {
		assert_eq!(parse_number("0x12"), Ok((f(18f64), 4)));
		assert_eq!(parse_number("0X12"), Ok((f(18f64), 4)));
		assert_eq!(parse_number("0x10Dc"), Ok((f(4316f64), 6)));

		assert_eq!(parse_number("0xFFFFFFFFFFFFFFFFFFFFFF"), Ok((f(3.094850098213451e+26), 24)));
	}

	#[test]
	fn octal() {
		assert_eq!(parse_number("0o777"), Ok((f(511f64), 5)));
		assert_eq!(parse_number("0O777"), Ok((f(511f64), 5)));

		assert_eq!(parse_number("0777"), Ok((f(511f64), 4)));
		assert_eq!(parse_number("0888"), Ok((f(888f64), 4)));
		assert_eq!(parse_number("012.5"), Ok((f(12.5f64), 5)));
	}

	#[test]
	fn binary() {
		assert_eq!(parse_number("0b1010"), Ok((f(10f64), 6)));
		assert_eq!(parse_number("0B1010"), Ok((f(10f64), 6)));
		assert_eq!(parse_number("0b1000_1010"), Ok((f(138f64), 11)));
	}

	#[test]
	fn numbers_invalid() {
		assert!(parse_number("0x.3888").is_err());

		assert!(parse_number("0x_0").is_err());
		assert!(parse_number("0x0_").is_err());
		assert!(parse_number("0__0").is_err());
		assert!(parse_number("0_.0").is_err());
		assert!(parse_number("0._0").is_err());
	}

	#[test]
	fn big_ints() {
		assert_eq!(parse_number("10n + "), Ok((n("10"), 3)));
		assert_eq!(parse_number("9007199254740991n"), Ok((n("9007199254740991"), 17)));
		assert_eq!(parse_number("0x1fffffffffffffn"), Ok((n("0x1fffffffffffff"), 17)));
		assert_eq!(parse_number("0o377777777777777777n"), Ok((n("0o377777777777777777"), 21)));
		assert_eq!(
			parse_number("0b11111111111111111111111111111111111111111111111111111n"),
			Ok((n("0b11111111111111111111111111111111111111111111111111111"), 56))
		);
	}
}

/// FUTURE Cow
#[derive(Debug, Clone)]
#[apply(crate::derive_ASTNode!)]
pub struct BigInt {
	/// This can contain prefix information etc
	pub source: String,
}

impl BigInt {
	#[must_use]
	pub fn radix(&self) -> u32 {
		let s = self.source.chars().nth(1);
		match s {
			Some('x' | 'X') => 16,
			Some('o' | 'O') => 8,
			Some('b' | 'B') => 2,
			_ => 1,
		}
	}

	#[must_use]
	pub fn value_and_radix(&self) -> (&str, u32) {
		let radix = self.radix();
		if radix == 1 { (&self.source, radix) } else { (&self.source[2..], radix) }
	}
}

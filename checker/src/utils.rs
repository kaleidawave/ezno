pub(crate) fn format_list<D: Display>(mut iterator: impl ExactSizeIterator<Item = D>) -> String {
	use std::fmt::Write;

	match iterator.len() {
		0 => "".into(),
		1 => iterator.next().unwrap().to_string(),
		2 => format!("{} and {}", iterator.next().unwrap(), iterator.next().unwrap()),
		val => {
			let mut buf = String::new();
			for value in iterator.by_ref().take(val - 1) {
				write!(&mut buf, "{}, ", value).unwrap();
			}
			write!(&mut buf, "and {}", iterator.next().unwrap()).unwrap();
			buf
		}
	}
}

// A trait for special handing when displaying user readable types
// pub trait TypeDisplay {
// 	/// TODO might need environment information rather than memory...?
// 	/// - type ids -> names (maybe that goes on memory..., prefer environment)
// 	/// -
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut HashSet<usize>,
// 		environment: &GeneralContext,
// 		store: &TypeStore,
// 	) {
// 		// TODO temp
// 		todo!("fmt not implemented")
// 	}

// 	fn to_string(&self, environment: &GeneralContext) -> String {
// 		let mut buf = String::new();
// 		TypeDisplay::fmt(self, &mut buf, 0, &mut HashSet::new(), environment);
// 		buf
// 	}
// }

// impl<T: TypeDisplay> TypeDisplay for Option<T> {
// 	fn fmt(
// 		&self,
// 		buf: &mut String,
// 		indent: usize,
// 		cycles: &mut HashSet<usize>,
// 		environment: &GeneralContext,
// 		store: &TypeStore,
// 	) {
// 		match self {
// 			Some(ty) => TypeDisplay::fmt(ty, buf, indent, cycles, environment),
// 			None => buf.push_str("any"),
// 		}
// 	}
// }

static IS_DEBUG_MODE: std::sync::Mutex<Option<bool>> = std::sync::Mutex::new(None);

pub(crate) fn is_debug_mode() -> bool {
	*IS_DEBUG_MODE.lock().unwrap().get_or_insert_with(|| {
		std::env::var("EZNO_DEBUG").map(|value| !value.is_empty()).unwrap_or_default()
	})
}

pub fn shorten(s: &str) -> &str {
	&s[s.find("src").expect("file not under 'src' folder")..]
}

macro_rules! notify {
    () => {
		if crate::utils::is_debug_mode() {
			#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
			eprintln!("[{}:{}]", crate::utils::shorten(file!()), line!())
		}
    };

    ($content:expr) => {
		if crate::utils::is_debug_mode() {
			#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
			eprintln!("[{}:{}] {}", crate::utils::shorten(file!()), line!(), $content)
		}
    };

    ($content:literal, $($es:expr),+) => {
		if crate::utils::is_debug_mode() {
			#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
			eprintln!("[{}:{}] {}", crate::utils::shorten(file!()), line!(), format_args!($content, $($es),+))
		}
    };
}
use std::fmt::Display;

pub(crate) use notify;

pub trait ExtendedZipTrait: Iterator + Sized {
	fn extended_zip<I: Iterator>(self, other_iterator: I) -> ExtendedZip<Self, I>;
}

impl<T: Iterator> ExtendedZipTrait for T {
	fn extended_zip<I: Iterator>(self, other_iterator: I) -> ExtendedZip<Self, I> {
		ExtendedZip(self, other_iterator)
	}
}

pub struct ExtendedZip<I1: Iterator, I2: Iterator>(pub(crate) I1, pub(crate) I2);

impl<I1: Iterator, I2: Iterator> Iterator for ExtendedZip<I1, I2> {
	type Item = (Option<I1::Item>, Option<I2::Item>);

	fn next(&mut self) -> Option<(Option<I1::Item>, Option<I2::Item>)> {
		let res1 = self.0.next();
		let res2 = self.1.next();
		if res1.is_none() && res2.is_none() {
			None
		} else {
			Some((res1, res2))
		}
	}
}

#[derive(Debug)]
pub enum EnforcedOr<T> {
	LeftAndRight { left: T, right: T },
	Left(T),
	Right(T),
}

pub trait EnforcedOrExt<T> {
	type Result;

	fn and_enforced(self, other: Self) -> Self::Result;
}

impl<T> EnforcedOrExt<T> for Option<T> {
	type Result = Option<EnforcedOr<T>>;

	fn and_enforced(self, other: Self) -> Self::Result {
		match (self, other) {
			(None, None) => None,
			(None, Some(right)) => Some(EnforcedOr::Right(right)),
			(Some(left), None) => Some(EnforcedOr::Left(left)),
			(Some(left), Some(right)) => Some(EnforcedOr::LeftAndRight { left, right }),
		}
	}
}

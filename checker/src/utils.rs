static IS_DEBUG_MODE: std::sync::Mutex<Option<bool>> = std::sync::Mutex::new(None);

pub(crate) fn is_debug_mode() -> bool {
	*IS_DEBUG_MODE.lock().unwrap().get_or_insert_with(|| {
		std::env::var("EZNO_DEBUG").map(|value| !value.is_empty()).unwrap_or_default()
	})
}

/// For `notify!` macro below
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

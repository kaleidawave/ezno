static IS_DEBUG_MODE: std::sync::Mutex<Option<bool>> = std::sync::Mutex::new(None);

#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
#[allow(clippy::manual_is_variant_and)]
pub(crate) fn is_debug_mode() -> bool {
	*IS_DEBUG_MODE.lock().unwrap().get_or_insert_with(|| {
		std::env::var("EZNO_DEBUG").map(|value| !value.is_empty()).unwrap_or_default()
	})
}

#[cfg(any(not(debug_assertions), target_arch = "wasm32"))]
pub(crate) fn is_debug_mode() -> bool {
	false
}

/// For `notify!` macro below
pub fn shorten(s: &str) -> &str {
	&s[s.find("src").expect("file not under 'src' folder")..]
}

macro_rules! notify {
    () => {
		if crate::utilities::is_debug_mode() {
			eprintln!("[{}:{}]", crate::utilities::shorten(file!()), line!())
		}
    };

    ($content:expr) => {
		if crate::utilities::is_debug_mode() {
			eprintln!("[{}:{}] {}", crate::utilities::shorten(file!()), line!(), $content)
		}
    };

    ($content:literal, $($es:expr),+) => {
		if crate::utilities::is_debug_mode() {
			eprintln!("[{}:{}] {}", crate::utilities::shorten(file!()), line!(), format_args!($content, $($es),+))
		}
    };
}

#[doc(hidden)]
pub(crate) use notify;

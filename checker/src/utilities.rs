static IS_DEBUG_MODE: std::sync::Mutex<Option<bool>> = std::sync::Mutex::new(None);

#[allow(clippy::manual_is_variant_and)]
pub(crate) fn is_debug_mode() -> bool {
	if cfg!(all(debug_assertions, not(target_arch = "wasm32"))) {
		*IS_DEBUG_MODE.lock().unwrap().get_or_insert_with(|| {
			std::env::var("EZNO_DEBUG").map(|value| !value.is_empty()).unwrap_or_default()
		})
	} else {
		false
	}
}

pub(crate) fn _set_debug_mode(value: bool) {
	if cfg!(all(debug_assertions, not(target_arch = "wasm32"))) {
		let _ = *IS_DEBUG_MODE.lock().unwrap().insert(value);
	}
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

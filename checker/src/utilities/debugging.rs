use std::cell::Cell;

// For `notify!` macro below. This caches the value looked up by std::env::var(...)
thread_local! {
	static IS_DEBUG_MODE: Cell<Option<bool>> = const { Cell::new(None) };
	static DEBUG_MODE_PAUSED: Cell<bool> = const { Cell::new(false) };
}

#[allow(clippy::manual_is_variant_and)]
pub(crate) fn is_debug_mode() -> bool {
	if cfg!(all(debug_assertions, not(target_arch = "wasm32"))) {
		if DEBUG_MODE_PAUSED.get() {
			return false;
		}
		let value = IS_DEBUG_MODE.get();
		if let Some(value) = value {
			value
		} else {
			let new_value = std::env::var("EZNO_DEBUG")
				.map(|value| !(value.is_empty() || value == "0"))
				.unwrap_or_default();
			IS_DEBUG_MODE.set(Some(new_value));
			new_value
		}
	} else {
		false
	}
}

pub(crate) fn pause_debug_mode() {
	if cfg!(all(debug_assertions, not(target_arch = "wasm32"))) {
		DEBUG_MODE_PAUSED.set(true);
	}
}

pub(crate) fn unpause_debug_mode() {
	if cfg!(all(debug_assertions, not(target_arch = "wasm32"))) {
		DEBUG_MODE_PAUSED.set(false);
	}
}

/// For `notify!` macro below
#[must_use]
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

macro_rules! add_timing {
	($chronometer:expr, $field:ident, $now:expr) => {
		if let Some(now) = $now {
			$chronometer.$field += now.elapsed();
		}
	};
}

#[doc(hidden)]
pub(crate) use add_timing;

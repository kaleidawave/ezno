//! These should probably be made into reusable crates at some point

mod debugging;
pub mod float_range;
pub mod map;
pub mod modulo_class;
pub mod range_map;
pub mod serialization;

pub use debugging::*;

#[must_use]
pub fn get_closest<'a, 'b>(
	items: impl Iterator<Item = &'a str>,
	closest_one: &'b str,
) -> Option<Vec<&'a str>> {
	const MIN_DISTANCE: usize = 2;
	let candidates = items
		.filter(|item| levenshtein::levenshtein(closest_one, item) <= MIN_DISTANCE)
		.collect::<Vec<&str>>();
	match candidates.len() {
		0 => None,
		1.. => Some(candidates),
	}
}

#[must_use]
pub fn get_possibles_message(possibles: &[&str]) -> String {
	match possibles {
		// Warning?
		[] => String::new(),
		[a] => format!("Did you mean '{a}'?"),
		[a @ .., b] => {
			let mut iter = a.iter();
			let first = format!("'{first}'", first = iter.next().unwrap());
			format!(
				"Did you mean {items} or '{b}'?",
				items = iter.fold(first, |acc, item| format!("{acc}, '{item}'"))
			)
		}
	}
}

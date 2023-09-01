use std::cell::RefCell;

use crate::Environment;
use super::facts::Facts;



pub struct CheckThings;

impl CallCheckingBehavior for CheckThings {
	const CHECK_TYPES: bool = true;

	fn get_top_level_facts<'a>(&'a mut self, environment: &'a mut Environment) -> &'a mut Facts {
		&mut environment.facts
	}
}

/// For anything that might involve a call, including gets, sets and actual calls
pub(crate) trait CallCheckingBehavior {
	// TODO
	const CHECK_TYPES: bool;

	fn get_top_level_facts<'a>(&'a mut self, environment: &'a mut Environment) -> &'a mut Facts;
}

pub(crate) enum TargetType {
	/// TODO currently refcell to
	Conditional(RefCell<Facts>),
	/// TODO function id for recursion
	Function(()),
}

pub(crate) struct Target {
	// pub(crate) based_on: &'a mut Environment<'a>,
	// pub(crate) chain: Annex<'a, Vec<TargetType>>,
	pub(crate) facts: Option<Facts>,
}

impl CallCheckingBehavior for Target {
	const CHECK_TYPES: bool = false;

	fn get_top_level_facts<'a>(&'a mut self, environment: &'a mut Environment) -> &'a mut Facts {
		self.facts.as_mut().unwrap_or(&mut environment.facts)
	}
}

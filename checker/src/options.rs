use std::any::Any;

/// Settings for type checking
/// TODO reach compat with tsc
#[derive(serde::Deserialize)]
pub struct TypeCheckOptions {
	/// Parameters cannot be reassigned
	pub constant_parameters: bool,
	/// Missing arguments are treated as undefined (thats how JS works)
	pub allow_elided_arguments: bool,
	pub allow_extra_arguments: bool,
	pub constant_function_declarations: bool,
	/// Whether auto casts can happen
	/// TODO maybe levels
	pub strict_casts: bool,
	/// Any types displayed will be in debug view
	pub debug_types: bool,
}

impl Default for TypeCheckOptions {
	fn default() -> Self {
		Self {
			constant_parameters: false,
			allow_elided_arguments: false,
			allow_extra_arguments: false,
			constant_function_declarations: true,
			// TODO middle value
			strict_casts: false,
			debug_types: false,
		}
	}
}

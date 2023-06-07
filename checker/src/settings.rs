/// Settings for type checking
/// TODO from toml, yml and json
/// TODO reach compat with tsc
#[derive(Clone, Debug, serde::Deserialize)]
pub struct TypeCheckSettings {
	pub variable_mangling: bool,
	/// Will pause on "debugger" statements and interactive environment explorer for debugging
	/// Parameters cannot be reassigned
	pub constant_parameters: bool,
	/// Missing arguments are treated as undefined (thats how JS works)
	pub allow_elided_arguments: bool,
	pub allow_extra_arguments: bool,
	pub constant_function_declarations: bool,
	/// TODO for repl
	pub extract_functions: bool,
	/// Whether auto casts can happen
	/// TODO maybe levels
	pub strict_casts: bool,
	/// Any types displayed will be in debug view
	pub debug_types: bool,
}

impl Default for TypeCheckSettings {
	fn default() -> Self {
		Self {
			variable_mangling: false,
			constant_parameters: false,
			allow_elided_arguments: false,
			allow_extra_arguments: false,
			constant_function_declarations: true,
			extract_functions: true,
			// TODO middle value
			strict_casts: false,
			debug_types: false,
		}
	}
}

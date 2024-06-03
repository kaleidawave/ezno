/// Options for type checking
/// TODO figure out compat with tsc
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
#[allow(clippy::struct_excessive_bools)]
pub struct TypeCheckOptions {
	/// Parameters cannot be reassigned
	pub constant_parameters: bool,

	/// Missing arguments are treated as undefined (thats how JS works)
	pub allow_elided_arguments: bool,

	/// Addition arguments are allowed
	pub allow_extra_arguments: bool,

	/// Given a `function x`, `x = 2` is not possible
	pub constant_function_declarations: bool,

	/// Whether auto casts can happen. aka `{} + 2` is allowed using the Object's primitive default
	/// TODO maybe levels
	pub strict_casts: bool,

	/// Any types displayed will be in debug view
	pub debug_types: bool,

	/// Enables `as` casts
	pub allow_cast: bool,

	/// For post type check optimisations and LSP. Stores both expressions and type annotations
	pub store_type_mappings: bool,

	/// TODO WIP
	pub parse_comments: bool,

	/// Allows partial syntax and collects other information for using in editor
	pub lsp_mode: bool,

	/// Can be used for linting
	pub record_all_assignments_and_reads: bool,

	/// Technically the `i` in `for (let i = 0; i < ...)` can be reassigned to `any` type. But this behavior isn't great
	/// and adds work for the inference engine. So this instead picks a basic type instead. This will
	/// raise errors in valid javascript
	pub infer_sensible_constraints_in_for_loops: bool,
}

impl Default for TypeCheckOptions {
	fn default() -> Self {
		Self {
			constant_parameters: false,
			allow_elided_arguments: false,
			allow_extra_arguments: false,
			constant_function_declarations: true,
			debug_types: false,
			parse_comments: true,
			strict_casts: false,
			store_type_mappings: false,
			lsp_mode: false,
			record_all_assignments_and_reads: false,
			infer_sensible_constraints_in_for_loops: true,
			// TODO false at some point hopefully!
			allow_cast: true,
		}
	}
}

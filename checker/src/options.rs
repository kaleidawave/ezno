/// Options for type checking
/// TODO figure out compat with tsc
#[cfg_attr(feature = "serde", derive(serde::Deserialize))]
#[allow(clippy::struct_excessive_bools)]
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

	/// For post type check optimisations and
	pub store_expression_type_mappings: bool,

	/// TODO WIP
	pub parse_comments: bool,
	/// Allows partial syntax and collects other information for using in editor
	pub lsp_mode: bool,
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
			store_expression_type_mappings: false,
			lsp_mode: false,
		}
	}
}

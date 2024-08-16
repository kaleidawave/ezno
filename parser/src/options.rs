use super::lexer::LexerOptions;

/// Options to customize parsing
#[allow(unused)]
#[derive(Copy, Clone)]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ParseOptions {
	/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
	pub jsx: bool,
	/// allow type annotations
	pub type_annotations: bool,
	/// just definition file
	pub type_definition_module: bool,
	/// Allow custom characters in JSX attributes
	pub special_jsx_attributes: bool,
	/// Parses decorators on items
	pub decorators: bool,
	/// Skip **all** comments from the AST
	pub comments: Comments,
	/// See [`crate::extensions::is_expression::IsExpression`]
	pub is_expressions: bool,
	/// Allows functions to be prefixed with 'server'
	pub custom_function_headers: bool,
	/// TODO temp for seeing how channel performs
	pub buffer_size: usize,
	/// Has no effect on WASM. Increase for deeply nested AST structures
	pub stack_size: Option<usize>,
	/// Useful for LSP information
	pub record_keyword_positions: bool,
	/// For the generator
	pub interpolation_points: bool,
	/// Extra
	pub destructuring_type_annotation: bool,
	/// Extra
	pub extra_operators: bool,
	/// For formatting
	pub retain_blank_lines: bool,
	/// For LSP
	pub partial_syntax: bool,
	/// JSX with modifications equiv
	pub top_level_html: bool,
}

impl ParseOptions {
	pub(crate) fn get_lex_options(&self) -> LexerOptions {
		LexerOptions {
			comments: self.comments,
			lex_jsx: self.jsx,
			allow_unsupported_characters_in_jsx_attribute_keys: self.special_jsx_attributes,
			allow_expressions_in_jsx: !self.top_level_html,
			top_level_html: self.top_level_html,
		}
	}

	#[must_use]
	pub fn all_features() -> Self {
		Self {
			jsx: true,
			type_annotations: true,
			type_definition_module: false,
			special_jsx_attributes: true,
			comments: Comments::All,
			decorators: true,
			custom_function_headers: true,
			is_expressions: true,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: true,
			// Only used in the AST-generator
			interpolation_points: false,
			partial_syntax: true,
			destructuring_type_annotation: true,
			extra_operators: true,
			retain_blank_lines: true,
			top_level_html: false,
		}
	}
}

// TODO unsure about some of these defaults, may change in future
impl Default for ParseOptions {
	fn default() -> Self {
		Self {
			jsx: true,
			type_annotations: true,
			type_definition_module: false,
			special_jsx_attributes: false,
			comments: Comments::All,
			decorators: true,
			custom_function_headers: false,
			is_expressions: false,
			buffer_size: 100,
			stack_size: None,
			record_keyword_positions: false,
			interpolation_points: false,
			partial_syntax: false,
			destructuring_type_annotation: false,
			extra_operators: false,
			retain_blank_lines: false,
			top_level_html: false,
		}
	}
}

/// Settings for serializing `ASTNodes`
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ToStringOptions {
	/// Does not include whitespace minification
	pub pretty: bool,
	/// Blocks have trailing semicolons. Has no effect if pretty == false
	pub trailing_semicolon: bool,
	/// Single statements get put on the same line as their parent statement
	pub single_statement_on_new_line: bool,
	/// Include type annotations (and additional TypeScript) syntax
	pub include_type_annotations: bool,
	/// TODO unsure about this
	pub include_decorators: bool,
	pub comments: Comments,
	pub indent_with: String,
	/// If false, panics if sees JSX
	pub expect_jsx: bool,
	/// For partial AST, marker nodes may exist. This allows pretty printing on invalid source
	/// but should be `false` for builds
	///
	/// if `false` and a marker node is found, printing will panic
	pub expect_markers: bool,
	/// has no effect under !pretty
	pub max_line_length: u8,
}

impl Default for ToStringOptions {
	fn default() -> Self {
		ToStringOptions {
			pretty: true,
			include_type_annotations: false,
			single_statement_on_new_line: true,
			include_decorators: false,
			comments: Comments::All,
			expect_jsx: false,
			trailing_semicolon: false,
			expect_markers: false,
			indent_with: "\t".to_owned(),
			max_line_length: u8::MAX,
		}
	}
}

impl ToStringOptions {
	#[must_use]
	pub fn minified() -> Self {
		ToStringOptions {
			pretty: false,
			comments: Comments::None,
			indent_with: String::new(),
			..Default::default()
		}
	}

	/// With TypeScript type syntax
	#[must_use]
	pub fn typescript() -> Self {
		ToStringOptions { include_type_annotations: true, ..Default::default() }
	}

	/// Whether to include comment in source
	pub(crate) fn should_add_comment(&self, content: &str) -> bool {
		self.comments.should_add_comment(content)
	}

	pub(crate) fn add_indent<T: source_map::ToString>(&self, indent: u8, buf: &mut T) {
		if self.pretty {
			(0..indent).for_each(|_| buf.push_str(&self.indent_with));
		}
	}

	/// Adds whitespace **conditionally** (based on pretty setting)
	pub(crate) fn push_gap_optionally<T: source_map::ToString>(&self, buf: &mut T) {
		if self.pretty {
			buf.push(' ');
		}
	}

	pub(crate) fn enforce_limit_length_limit(&self) -> bool {
		self.pretty && self.max_line_length != u8::MAX
	}
}

#[derive(Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub enum Comments {
	#[default]
	All,
	/// Only multiline comments starting with `/**`
	JustDocumentation,
	None,
}

impl Comments {
	/// Whether to include comment in source
	pub(crate) fn should_add_comment(self, content: &str) -> bool {
		match self {
			Comments::All => true,
			Comments::None => false,
			Comments::JustDocumentation => {
				content.starts_with('*') || content.trim_start().starts_with('@')
			}
		}
	}
}

#[derive(Copy, Clone, Default)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub enum TypeAnnotationOption {
	/// They are parser but considered as an error
	AsErrors,
	#[default]
	Allowed,
	/// for definition module. this has some effect on parsing
	Definitions,
}

impl TypeAnnotationOption {
	pub fn type_annotations(&self) -> bool {
		matches!(self, Self::Allowed | Self::Definitions)
	}

	pub fn is_definition_file(&self) -> bool {
		matches!(self, Self::Definitions)
	}
}

/// TODO could split up even more
/// TODO Can be refactored with bit to reduce memory
#[derive(Copy, Clone, Default)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct ParseOptions {
	pub type_annotations: TypeAnnotationOption,
	pub comments: CommentsOption,
	/// None => Disabled
	pub jsx: Option<JSXOptions>,
	pub decorators: bool,
	// ---
	pub extras: Extras,
	pub features: Features,
}

/// TODO JSX, TypeScript, decorators etc
#[derive(Copy, Clone, Default)]
#[expect(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct Extras {
	pub is_expressions: bool,
	pub enum_members_as_data_types: bool,
	pub extra_operators: bool,
	/// This breaks JavaScript
	pub destructuring_type_annotation: bool,
	pub custom_function_headers: bool,
	pub reversed_imports: bool,
	/// Enables single parameter annotations, class + fields and symbol names
	pub additional_type_annotations: bool,
}

impl Extras {
	pub fn all() -> Self {
		Self {
			is_expressions: true,
			enum_members_as_data_types: true,
			extra_operators: true,
			destructuring_type_annotation: true,
			custom_function_headers: true,
			reversed_imports: true,
			additional_type_annotations: true,
		}
	}
}

#[derive(Copy, Clone, Default)]
#[expect(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct Features {
	/// Allows missing syntax in certain positions (see specification)
	pub partial_syntax: bool,
	/// Certain expressions
	pub interpolation_points: bool,
	pub record_keyword_positions: bool,
	/// For formatting
	pub retain_blank_lines: bool,
	pub run_validation: bool,
}

/// Parsing of [JSX](https://facebook.github.io/jsx/) (includes some additions)
#[allow(unused)]
#[derive(Copy, Clone, Default)]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Deserialize), serde(default))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct JSXOptions {
	/// Allow custom characters in JSX attributes
	pub special_jsx_attributes: bool,
	/// JSX with modifications
	pub top_level_html: bool,
	/// Attribute values are JavaScript expressions (strings are retained, but unquoted items
	/// become variable references)
	pub attributes_as_expressions: bool,
}

impl ParseOptions {
	#[must_use]
	pub fn all() -> Self {
		Self {
			type_annotations: TypeAnnotationOption::Allowed,
			comments: CommentsOption::All,
			jsx: Some(JSXOptions {
				special_jsx_attributes: true,
				top_level_html: true,
				attributes_as_expressions: true,
			}),
			decorators: true,
			extras: Extras {
				is_expressions: true,
				enum_members_as_data_types: true,
				extra_operators: true,
				destructuring_type_annotation: true,
				custom_function_headers: true,
				reversed_imports: true,
				additional_type_annotations: true,
			},
			features: Features::default(),
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
	pub comments: CommentsOption,
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
			comments: CommentsOption::All,
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
			comments: CommentsOption::None,
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
pub enum CommentsOption {
	#[default]
	All,
	/// Only multiline comments starting with `/**`
	JustDocumentation,
	None,
}

impl CommentsOption {
	/// Whether to include comment in source
	pub(crate) fn should_add_comment(self, content: &str) -> bool {
		match self {
			CommentsOption::All => true,
			CommentsOption::None => false,
			CommentsOption::JustDocumentation => {
				content.starts_with('*') || content.trim_start().starts_with('@')
			}
		}
	}
}

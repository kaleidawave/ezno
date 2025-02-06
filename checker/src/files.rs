use super::{CheckingData, Environment, LocalInformation, Names, ReadFromFS, RootContext, TypeId};
use source_map::{FileSystem, MapFileStore, Nullable, SourceId, Span, SpanWithSource, WithPathMap};
use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

pub(super) fn parse_source<T: ReadFromFS, A: ASTImplementation>(
	path: &Path,
	source: SourceId,
	content: String,
	checking_data: &mut CheckingData<T, A>,
) -> Result<<A as ASTImplementation>::Module<'static>, <A as ASTImplementation>::ParseError> {
	if checking_data.options.measure_time {
		let code_lines =
			content.lines().filter(|c| !(c.is_empty() || c.trim_start().starts_with('/'))).count();
		checking_data.yardstick.lines += code_lines;
	}

	// TODO pause check timing
	let current = checking_data.options.measure_time.then(std::time::Instant::now);

	// TODO abstract using similar to import logic
	let is_js = path.extension().and_then(|s| s.to_str()).is_some_and(|s| s.ends_with("js"));

	let parse_options = A::parse_options(
		is_js,
		checking_data.options.extra_syntax,
		checking_data.options.parse_comments,
		checking_data.options.lsp_mode,
	);

	let result = A::module_from_string(
		source,
		content,
		parse_options,
		&mut checking_data.modules.parser_requirements,
	);

	if let Some(current) = current {
		checking_data.chronometer.parse += current.elapsed();
	}

	result
}

pub trait ASTImplementation: Sized {
	type ParseOptions;
	/// Custom allocator etc
	type ParserRequirements;

	type ParseError: Into<crate::Diagnostic>;

	type Module<'a>;
	/// TODO temp item. Some modules can have references
	type OwnedModule;

	type DefinitionFile<'a>;

	type TypeAnnotation<'a>;
	type TypeParameter<'a>;
	type Expression<'a>;
	/// List of statements and declarations
	type Block<'a>;
	type MultipleExpression<'a>;
	type ForStatementInitiliser<'a>;

	/// Used in `for of`, `for in` and function parameters
	type VariableField<'a>;

	type ClassMethod<'a>: SynthesisableFunction<Self>;

	fn module_from_string(
		source_id: SourceId,
		string: String,
		options: Self::ParseOptions,
		parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::Module<'static>, Self::ParseError>;

	fn definition_module_from_string(
		source_id: SourceId,
		string: String,
		parser_requirements: &mut Self::ParserRequirements,
	) -> Result<Self::DefinitionFile<'static>, Self::ParseError>;

	#[allow(clippy::needless_lifetimes)]
	fn synthesise_module<'a, T: ReadFromFS>(
		module: &Self::Module<'a>,
		source_id: SourceId,
		module_context: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	#[allow(clippy::needless_lifetimes)]
	fn synthesise_definition_module<'a, T: ReadFromFS>(
		module: &Self::DefinitionFile<'a>,
		source: SourceId,
		root: &RootContext,
		checking_data: &mut CheckingData<T, Self>,
	) -> (Names, LocalInformation);

	/// Expected is used for eagerly setting function parameters
	fn synthesise_expression<T: ReadFromFS>(
		expression: &Self::Expression<'_>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	/// Expected is used for eagerly setting function parameters
	fn synthesise_multiple_expression<'a, T: ReadFromFS>(
		expression: &'a Self::MultipleExpression<'a>,
		expected_type: TypeId,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_parameter_extends<T: ReadFromFS>(
		parameter: &Self::TypeParameter<'_>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_type_annotation<'a, T: ReadFromFS>(
		annotation: &'a Self::TypeAnnotation<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	) -> TypeId;

	fn synthesise_block<'a, T: ReadFromFS>(
		block: &'a Self::Block<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	/// Don't need to return anything. All information recorded via changed to `environment`
	fn synthesise_for_loop_initialiser<'a, T: ReadFromFS>(
		for_loop_initialiser: &'a Self::ForStatementInitiliser<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
	);

	fn expression_position<'a>(expression: &'a Self::Expression<'a>) -> Span;

	fn multiple_expression_position<'a>(expression: &'a Self::MultipleExpression<'a>) -> Span;

	fn type_parameter_name<'a>(parameter: &'a Self::TypeParameter<'a>) -> &'a str;

	fn type_annotation_position<'a>(annotation: &'a Self::TypeAnnotation<'a>) -> Span;

	fn parameter_constrained<'a>(parameter: &'a Self::TypeParameter<'a>) -> bool;

	#[allow(clippy::fn_params_excessive_bools)]
	fn parse_options(
		is_js: bool,
		extra_syntax: bool,
		parse_comments: bool,
		lsp_mode: bool,
	) -> Self::ParseOptions;

	fn owned_module_from_module(m: Self::Module<'static>) -> Self::OwnedModule;

	/// For `for in` and `for of loops`
	fn declare_and_assign_to_fields<'a, T: ReadFromFS>(
		field: &'a Self::VariableField<'a>,
		environment: &mut Environment,
		checking_data: &mut crate::CheckingData<T, Self>,
		arguments: crate::context::VariableRegisterArguments,
	);
}

/// Covers both actual functions and
pub trait SynthesisableFunction<A: crate::ASTImplementation> {
	fn id(&self, source_id: SourceId) -> crate::FunctionId {
		crate::FunctionId(source_id, self.get_position().start)
	}

	/// For debugging only
	fn get_name(&self) -> Option<&str>;

	/// For debugging only
	fn get_position(&self) -> source_map::Span;

	// TODO temp
	fn has_body(&self) -> bool;

	// /// For detecting what is inside
	// fn get_body_span(&self) -> source_map::Span;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn type_parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<crate::types::functions::GenericTypeParameters>;

	fn this_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<TypeId>;

	/// For object literals
	fn super_constraint<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<TypeId>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESISING THEM**
	fn parameters<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
		expected_parameters: Option<&crate::types::functions::SynthesisedParameters>,
	) -> crate::types::functions::SynthesisedParameters;

	fn return_type_annotation<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) -> Option<crate::types::functions::ReturnType>;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	);
}

/// Contains all the modules and mappings for import statements
///
/// TODO could files and `synthesised_modules` be merged? (with a change to the source map crate)
pub struct ModuleData<AST: ASTImplementation> {
	pub(crate) parser_requirements: AST::ParserRequirements,
	pub(crate) current_working_directory: PathBuf,
	/// Contains the text content of files (for source maps and diagnostics)
	pub(crate) files: MapFileStore<WithPathMap>,
	/// To catch cyclic imports
	pub(crate) _currently_checking_modules: std::collections::HashSet<PathBuf>,
	/// The result of checking. Includes exported variables and info
	pub(crate) synthesised_modules: HashMap<SourceId, crate::SynthesisedModule<AST::OwnedModule>>,
}

impl<A> ModuleData<A>
where
	A: ASTImplementation,
{
	pub(crate) fn new(
		current_working_directory: PathBuf,
		files: Option<MapFileStore<WithPathMap>>,
		parser_requirements: A::ParserRequirements,
	) -> Self {
		Self {
			files: files.unwrap_or_default(),
			synthesised_modules: Default::default(),
			_currently_checking_modules: Default::default(),
			// custom_module_resolvers,
			current_working_directory,
			parser_requirements,
		}
	}

	pub(crate) fn get_file<T: ReadFromFS>(
		&mut self,
		resolver: &mut T,
		path: &Path,
		chronometer: Option<&mut super::Chronometer>,
	) -> Option<File> {
		// TODO only internal code should be able to do this
		if let Some("bin") = path.extension().and_then(|s| s.to_str()) {
			return resolver.read_file(path).map(|s| File::Binary(s.clone()));
		}

		let get_source_at_path = self.files.get_source_at_path(path);

		// eprintln!(
		// 	"Found {:?} {:?} {:?}",
		// 	get_source_at_path,
		// 	path.display(),
		// 	self.files.get_paths()
		// );

		if let Some(source) = get_source_at_path {
			Some(File::Source(source, self.files.get_file_content(source)))
		} else {
			// Load into system
			let current = chronometer.is_some().then(std::time::Instant::now);
			let content = resolver.read_file(path)?;
			if let Ok(content) = String::from_utf8(content) {
				if let Some(current) = current {
					chronometer.unwrap().fs += current.elapsed();
				}
				let source_id = self.files.new_source_id(path.to_path_buf(), content);
				Some(File::Source(source_id, self.files.get_file_content(source_id)))
			} else {
				eprintln!("{} is not valid Utf-8", path.display());
				None
			}
		}
	}
}

pub enum File {
	Binary(Vec<u8>),
	Source(SourceId, String),
}

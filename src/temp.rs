use std::path::PathBuf;

use parser::{
	expressions::ExpressionId, source_map::MapFileStore, ASTNode, ParseSettings, SourceId,
	ToStringSettings,
};

use crate::error_handling::{self, TempDiagnostic};

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct Output {
	pub output_path: String,
	pub content: String,
	pub mappings: String,
}

#[cfg_attr(target_family = "wasm", derive(serde::Serialize))]
pub struct BuildOutput {
	pub outputs: Vec<Output>,
	pub temp_warnings_and_infos: Vec<TempDiagnostic>,
}

/// Just builds one file temporarily
pub fn build<T: crate::FSResolver>(
	_fs_resolver: T,
	content: String,
	input_path: String,
	output_path: String,
) -> (MapFileStore, Result<BuildOutput, Vec<TempDiagnostic>>) {
	let mut fs = MapFileStore::default();
	let source_id = SourceId::new(&mut fs, PathBuf::from(input_path), content.clone());
	let module_result = parser::Module::from_string(
		content,
		ParseSettings::default(),
		source_id,
		None,
		Default::default(),
	);

	let mut output = match module_result {
		Ok(t) => t,
		Err(parse_err) => {
			let diag = TempDiagnostic {
				label: parse_err.reason,
				position: parse_err.position,
				kind: error_handling::ErrorWarningInfo::Error,
			};
			return (fs, Err(vec![diag]));
		}
	};

	let mut visitors_mut = parser::VisitorsMut {
		expression_visitors_mut: vec![Box::new(InvertTernaryBranches)],
		..Default::default()
	};

	let mut temp_errors = Vec::new();

	output.0.visit_mut(
		&mut visitors_mut,
		&mut temp_errors,
		&mut output.1.function_extractor,
		&parser::VisitSettings::default(),
	);

	let (content, source_map) = output.0.to_string_with_source_map(
		&parser::ToStringSettingsAndData(ToStringSettings::minified(), output.1.function_extractor),
		&fs,
	);

	let output = Output { output_path, content, mappings: source_map.mappings };

	(fs, Ok(BuildOutput { outputs: vec![output], temp_warnings_and_infos: temp_errors }))
}

struct InvertTernaryBranches;

impl parser::VisitorMut<parser::Expression, Vec<TempDiagnostic>> for InvertTernaryBranches {
	fn visit_mut(
		&mut self,
		item: &mut parser::Expression,
		data: &mut Vec<TempDiagnostic>,
		_functions: &mut parser::extractor::ExtractedFunctions,
		_chain: &parser::Chain,
	) {
		if let parser::Expression::TernaryExpression {
			condition,
			truthy_result,
			falsy_result,
			..
		} = item
		{
			if let parser::Expression::UnaryOperation {
				operator: parser::operators::UnaryOperator::LogicalNot,
				operand,
				position,
				..
			} = condition.as_mut()
			{
				// Important that this is run before as changing AST changes the position
				let position = position.union(&falsy_result.get_position());

				data.push(TempDiagnostic {
					label: String::from("Ternary swapped"),
					position,
					kind: error_handling::ErrorWarningInfo::Info,
				});

				let temp_swap =
					parser::Expression::Null(parser::Span::NULL_SPAN, ExpressionId::NULL);

				*condition = Box::new(std::mem::replace(operand, temp_swap));
				std::mem::swap(truthy_result, falsy_result);
			}
		}
	}
}

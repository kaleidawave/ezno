use either::Either;
use serde::Serialize;
use source_map::{SourceId, Span};
use std::{
	fmt::{self, Debug, Display},
	iter,
	path::PathBuf,
};

/// Contains information
#[derive(Serialize, Debug)]
// #[serde(tag = "type")]
pub enum Diagnostic {
	/// Does not have positional information
	Global(String),
	Position {
		reason: String,
		pos: Span,
	},
	PositionWithAdditionLabels {
		reason: String,
		pos: Span,
		labels: Vec<(String, Option<Span>)>,
	},
}

#[derive(serde::Serialize)]
#[serde(tag = "type")]
pub enum ErrorWarningInfo {
	/// For errors, will back out and not produce output
	Error(Diagnostic),
	/// For actionable warnings
	Warning(Diagnostic),
	/// For diagnostic, TODO maybe disabled via debug
	Info(Diagnostic),
	/// For things...
	Data(Box<dyn erased_serde::Serialize>),
}

impl ErrorWarningInfo {
	pub fn get_diagnostic(self) -> Option<Diagnostic> {
		if let Self::Error(diag) | Self::Warning(diag) | Self::Info(diag) = self {
			Some(diag)
		} else {
			None
		}
	}
}

impl Diagnostic {
	pub fn sources<'a>(&'a self) -> impl Iterator<Item = SourceId> + 'a {
		use either::{Left, Right};
		match self {
			Diagnostic::Global(_) => Left(Left(iter::empty())),
			Diagnostic::Position { reason: _, pos: span } => {
				Left(Right(iter::once(span.source_id)))
			}
			Diagnostic::PositionWithAdditionLabels { pos, labels, .. } => {
				Right(iter::once(pos.source_id).chain(
					labels.iter().flat_map(|(_, span)| span.as_ref().map(|span| span.source_id)),
				))
			}
		}
	}

	pub fn reason(&self) -> &str {
		match self {
			Diagnostic::Global(reason)
			| Diagnostic::Position { reason, .. }
			| Diagnostic::PositionWithAdditionLabels { reason, .. } => &reason,
		}
	}
}

#[derive(Default, serde::Serialize)]
#[serde(transparent)]
pub struct DiagnosticsContainer {
	container: Vec<ErrorWarningInfo>,
	// Quick way to check whether a error was added
	#[serde(skip_serializing)]
	has_error: bool,
}

impl DiagnosticsContainer {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn add_error<T: Into<Diagnostic>>(&mut self, error: T) {
		self.has_error = true;
		self.container.push(ErrorWarningInfo::Error(error.into()))
	}

	pub fn add_warning<T: Into<Diagnostic>>(&mut self, warning: T) {
		self.container.push(ErrorWarningInfo::Warning(warning.into()))
	}

	pub fn add_info<T: Into<Diagnostic>>(&mut self, info: T) {
		self.container.push(ErrorWarningInfo::Info(info.into()))
	}

	pub fn add_data(&mut self, data: Box<dyn erased_serde::Serialize>) {
		self.container.push(ErrorWarningInfo::Data(data))
	}

	pub fn has_error(&self) -> bool {
		self.has_error
	}

	pub fn sources<'a>(&'a self) -> impl Iterator<Item = SourceId> + 'a {
		self.container.iter().flat_map(|item| match item {
			ErrorWarningInfo::Error(diag)
			| ErrorWarningInfo::Warning(diag)
			| ErrorWarningInfo::Info(diag) => Either::Left(diag.sources()),
			ErrorWarningInfo::Data(_) => Either::Right(iter::empty()),
		})
	}

	pub fn into_iter(self) -> impl DoubleEndedIterator<Item = ErrorWarningInfo> {
		self.container.into_iter()
	}

	pub fn into_result(self) -> Result<Self, Self> {
		if self.has_error {
			Err(self)
		} else {
			Ok(self)
		}
	}
}

pub(super) use defined_errors_and_warnings::*;

use crate::{
	context::GeneralEnvironment,
	types::{printing::print_type, TypeId, TypeStore},
};

/// TODO could be more things, for instance a property missing etc
pub(crate) enum TypeStringRepresentation {
	Type(String),
}

impl TypeStringRepresentation {
	pub fn from_type_id(
		id: TypeId,
		env: &GeneralEnvironment,
		types: &TypeStore,
		debug_mode: bool,
	) -> Self {
		if debug_mode {
			todo!()
		// 	let ty = get_env!(env.get_type_by_id(id));
		// 	Self::Type(format!("{:#?}", ty))
		} else {
			let value = print_type(types, id, env);
			Self::Type(value)
		}
	}
}

impl Display for TypeStringRepresentation {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeStringRepresentation::Type(ty) => f.write_str(&ty),
		}
	}
}

pub(crate) struct NoEnvironmentSpecified;

impl From<NoEnvironmentSpecified> for Diagnostic {
	fn from(error: NoEnvironmentSpecified) -> Self {
		Diagnostic::Global("No environment".to_owned())
	}
}

// Contains known internal errors and warnings
// Contained here in a module to separate user facing
mod defined_errors_and_warnings {
	use source_map::Span;

	use crate::{
		structures::{self, operators::UnaryOperator},
		Diagnostic,
	};
	use std::path;

	use super::TypeStringRepresentation;

	/// Reasons for errors, intermediate type for generating [Diagnostic]s
	/// e.g. cannot Call, cannot equate, duplicate key etc
	pub(crate) enum TypeCheckError<'a> {
		PropertyDoesNotExist {
			property: TypeStringRepresentation,
			ty: TypeStringRepresentation,
			site: Span,
		},
		RestParameterAnnotationShouldBeArrayType(Span),
		/// TODO better name
		NonExistentType(String),
		CannotIndexType,
		CouldNotFindVariable {
			variable: &'a str,
			possibles: Vec<&'a str>,
			position: Span,
		},
		CouldNotFindType(&'a str, Span),
		TypeHasNoGenericParameters(String, Span),
		ArgumentDoesNotMatchParameter {
			parameter_type: TypeStringRepresentation,
			parameter_position: Span,
			argument_type: TypeStringRepresentation,
			argument_position: Span,
			restriction: Option<(Span, TypeStringRepresentation)>,
		},
		/// TODO this also covers readonly
		/// TODO should this contain information about the constant
		CannotAssignToConstant {
			variable_name: &'a str,
			variable_position: Span,
			assignment_position: Span,
		},
		CannotAssignToFrozen {
			variable_type: TypeStringRepresentation,
			assignment_position: Span,
		},
		InvalidAssignmentOrDeclaration {
			variable_type: TypeStringRepresentation,
			value_type: TypeStringRepresentation,
			variable_site: Span,
			value_site: Span,
		},
		InvalidJSXAttribute {
			attribute_name: String,
			attribute_type: TypeStringRepresentation,
			value_type: TypeStringRepresentation,
			// TODO
			attribute_type_site: (),
			value_site: Span,
		},
		InvalidComparison(TypeStringRepresentation, TypeStringRepresentation),
		InvalidAddition(TypeStringRepresentation, TypeStringRepresentation),
		/// Covers multiplication, subtraction, modulo etc
		/// TODO something better?
		InvalidMathematicalOperation(
			TypeStringRepresentation,
			TypeStringRepresentation,
			structures::operators::BinaryOperator,
			Span,
		),
		InvalidBitwiseOperation(
			TypeStringRepresentation,
			TypeStringRepresentation,
			structures::operators::BitwiseOperators,
		),
		ReturnedTypeDoesNotMatch {
			expected_return_type: TypeStringRepresentation,
			returned_type: TypeStringRepresentation,
			position: Span,
		},
		InvalidUnaryOperation(UnaryOperator, TypeStringRepresentation),
		// TODO are these the same errors?
		TypeIsNotIndexable(TypeStringRepresentation),
		TypeIsNotIterable(TypeStringRepresentation),
		// This could be a syntax error but that is difficult to type...
		NonTopLevelExport,
		// TODO implies the presence of, which isn't always true
		FieldNotExported(&'a str, &'a path::Path, Span),
		MissingArguments {
			function: TypeStringRepresentation,
			parameter_pos: Span,
			call_site: Span,
		},
		InvalidJSXInterpolatedValue {
			interpolation_site: Span,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		NotCallable {
			at: Span,
			calling: TypeStringRepresentation,
		},
		/// for the `satisfies` keyword
		NotSatisfied {
			at: Span,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		ExtraArgument {
			argument_position: Span,
		},
		Unsupported {
			thing: &'static str,
			at: Span,
		},
		NotWritable {
			pos: Span,
		},
		ValueDoesNotMeetConstraint {
			pos: Span,
			value_pos: Span,
		},
		ReDeclaredVariable {
			name: &'a str,
			pos: Span,
		},
		HiddenArgumentDoesNotMatch {
			identifier: String,
			requirement: TypeStringRepresentation,
			found: TypeStringRepresentation,
			call_site: Span,
		},
		/// TODO temp, needs more info
		FunctionDoesNotMeetConstraint {
			function_constraint: TypeStringRepresentation,
			function_type: TypeStringRepresentation,
			position: Span,
		},
		StatementsNotRun {
			between: Span,
		},
	}

	impl From<TypeCheckError<'_>> for Diagnostic {
		fn from(error: TypeCheckError<'_>) -> Self {
			match error {
				TypeCheckError::CouldNotFindVariable { variable, possibles, position } => {
					Diagnostic::Position {
						reason: format!(
							"Could not find variable {} in scope",
							variable,
							// possibles Consider '{:?}'
						),
						pos: position,
					}
				}
				TypeCheckError::CouldNotFindType(reference, pos) => Diagnostic::Position {
					reason: format!("Could not find type '{}'", reference),
					pos,
				},
				TypeCheckError::PropertyDoesNotExist { property, ty, site } => {
					Diagnostic::Position {
						reason: format!("No property with {} on {}", property, ty),
						pos: site,
					}
				}
				TypeCheckError::ArgumentDoesNotMatchParameter {
					parameter_type,
					argument_type,
					argument_position,
					parameter_position,
					restriction,
				} => {
					if let Some((restriction_pos, restriction)) = restriction {
						Diagnostic::PositionWithAdditionLabels {
							reason: format!(
								"Argument of type {} is not assignable to {}",
								argument_type, restriction
							),
							pos: argument_position,
							labels: vec![(
								format!(
									"Parameter {} was specialized with type {}",
									parameter_type, restriction
								),
								Some(restriction_pos),
							)],
						}
					} else {
						Diagnostic::PositionWithAdditionLabels {
							reason: format!(
								"Argument of type {} is not assignable to {}",
								argument_type, parameter_type
							),
							pos: argument_position,
							labels: vec![(
								format!("Parameter has type {}", parameter_type),
								Some(parameter_position),
							)],
						}
					}
				}
				TypeCheckError::InvalidAssignmentOrDeclaration {
					variable_type,
					value_type,
					value_site,
					variable_site,
				} => Diagnostic::PositionWithAdditionLabels {
					reason: format!(
						"Type {} is not assignable to type {}",
						value_type, variable_type
					),
					pos: value_site,
					labels: vec![(
						format!("Variable declared with type {}", variable_type),
						Some(variable_site),
					)],
				},
				TypeCheckError::InvalidJSXAttribute {
					attribute_name,
					attribute_type,
					value_type,
					attribute_type_site: variable_site,
					value_site,
				} => Diagnostic::Position {
					reason: format!(
						"Type {} is not assignable to {} attribute of type {}",
						attribute_name, value_type, attribute_type
					),
					pos: value_site,
				},
				TypeCheckError::ReturnedTypeDoesNotMatch {
					position,
					expected_return_type,
					returned_type,
				} => Diagnostic::Position {
					reason: format!(
						"Function is expected to return {} but returned {}",
						expected_return_type, returned_type
					),
					pos: position,
				},
				TypeCheckError::MissingArguments { function, parameter_pos, call_site } => {
					Diagnostic::PositionWithAdditionLabels {
						reason: format!("Calling {}, found missing arguments", function),
						pos: call_site,
						labels: vec![("Parameter defined here".to_owned(), Some(parameter_pos))],
					}
				}
				TypeCheckError::NonExistentType(_) => todo!(),
				TypeCheckError::CannotIndexType => todo!(),
				TypeCheckError::TypeHasNoGenericParameters(_, _) => todo!(),
				TypeCheckError::CannotAssignToConstant {
					variable_name,
					variable_position,
					assignment_position,
				} => Diagnostic::PositionWithAdditionLabels {
					reason: format!("Cannot reassign to constant variable '{}'", variable_name),
					pos: assignment_position,
					labels: vec![(
						"Constant variable defined here".to_owned(),
						Some(variable_position),
					)],
				},
				TypeCheckError::CannotAssignToFrozen { variable_type, assignment_position } => {
					Diagnostic::Position {
						reason: format!("{} is frozen", variable_type),
						pos: assignment_position,
					}
				}
				TypeCheckError::InvalidComparison(_, _) => todo!(),
				TypeCheckError::InvalidAddition(_, _) => todo!(),
				TypeCheckError::InvalidMathematicalOperation(lhs, rhs, operation, pos) => {
					Diagnostic::Position {
						reason: format!(
							"Invalid operation {}, between {} and {}",
							"todo", lhs, rhs
						),
						pos,
					}
				}
				TypeCheckError::InvalidBitwiseOperation(_, _, _) => todo!(),
				TypeCheckError::InvalidUnaryOperation(_, _) => todo!(),
				TypeCheckError::TypeIsNotIndexable(_) => todo!(),
				TypeCheckError::TypeIsNotIterable(_) => todo!(),
				TypeCheckError::NonTopLevelExport => todo!(),
				TypeCheckError::FieldNotExported(_, _, _) => todo!(),
				TypeCheckError::InvalidJSXInterpolatedValue {
					interpolation_site,
					expected,
					found,
				} => todo!(),
				TypeCheckError::RestParameterAnnotationShouldBeArrayType(pos) => {
					Diagnostic::Position {
						reason: "Rest parameter annotation should be array type".to_owned(),
						pos,
					}
				}
				TypeCheckError::Unsupported { thing, at } => {
					Diagnostic::Position { reason: format!("Unsupported: {}", thing), pos: at }
				}
				TypeCheckError::NotCallable { at, calling } => {
					Diagnostic::Position { reason: format!("Cannot call {}", calling), pos: at }
				}
				TypeCheckError::ExtraArgument { argument_position } => todo!(),
				TypeCheckError::NotWritable { pos } => Diagnostic::Position {
					reason: format!("Cannot assign to immutable property"),
					pos,
				},
				TypeCheckError::ValueDoesNotMeetConstraint { pos, value_pos } => {
					Diagnostic::Position {
						reason: format!("Assignment does not meet value constraint"),
						pos,
					}
				}
				TypeCheckError::ReDeclaredVariable { name, pos } => Diagnostic::Position {
					reason: format!("Cannot redeclare {} in scope", name),
					pos,
				},
				TypeCheckError::HiddenArgumentDoesNotMatch {
					identifier,
					requirement,
					found,
					call_site,
				} => Diagnostic::Position {
					reason: format!(
						"Calling function requires {} to be {}, found {}",
						identifier, requirement, found
					),
					pos: call_site,
				},
				TypeCheckError::FunctionDoesNotMeetConstraint {
					function_constraint,
					function_type,
					position,
				} => Diagnostic::Position {
					reason: format!(
						"{} constraint on function does not match synthesized form {}",
						function_constraint, function_type
					),
					pos: position,
				},
				TypeCheckError::StatementsNotRun { between } => Diagnostic::Position {
					reason: "Statements are never run".to_owned(),
					pos: between,
				},
				TypeCheckError::NotSatisfied { at, expected, found } => Diagnostic::Position {
					reason: format!("Expected {} found {}", expected, found),
					pos: at,
				},
			}
		}
	}

	pub enum TypeCheckWarning {
		AwaitUsedOnNonPromise(Span),
		/// TODO could be an error at some point
		DeadBranch {
			expression_span: Span,
			expression_value: bool,
		},
		IgnoringAsExpression(Span),
		Unsupported {
			thing: &'static str,
			at: Span,
		},
		UselessExpression {
			expression_span: Span,
			// TODO other branch information
		},
	}

	impl Into<Diagnostic> for TypeCheckWarning {
		fn into(self) -> Diagnostic {
			match self {
				TypeCheckWarning::AwaitUsedOnNonPromise(span) => Diagnostic::Position {
					reason: "Unnecessary await expression / type is not promise".to_owned(),
					pos: span,
				},
				TypeCheckWarning::DeadBranch { expression_span, expression_value } => {
					Diagnostic::Position {
						reason: format!("Expression is always {:?}", expression_value),
						pos: expression_span,
					}
				}
				TypeCheckWarning::IgnoringAsExpression(span) => Diagnostic::Position {
					reason: "'as' expressions are ignore by the checker".to_owned(),
					pos: span,
				},
				TypeCheckWarning::Unsupported { thing, at } => {
					Diagnostic::Position { reason: format!("Unsupported: {}", thing), pos: at }
				}
				TypeCheckWarning::UselessExpression { expression_span } => Diagnostic::Position {
					reason: "Expression is always true".to_owned(),
					pos: expression_span,
				},
			}
		}
	}
}

#[derive(Debug)]
pub struct CannotFindTypeError<'a>(pub &'a str);

#[derive(Debug)]
pub struct TypeIsNotIndexable<'a>(pub &'a TypeId);

pub(crate) struct TypeDefinitionModuleNotFound(pub PathBuf);

impl Into<Diagnostic> for TypeDefinitionModuleNotFound {
	fn into(self) -> Diagnostic {
		Diagnostic::Global(format!(
			"Could not find type definition module at '{}'",
			self.0.as_path().display()
		))
	}
}

pub(crate) struct EntryPointNotFound(pub PathBuf);

impl Into<Diagnostic> for EntryPointNotFound {
	fn into(self) -> Diagnostic {
		Diagnostic::Global(format!(
			"Could not entry point module at '{}'",
			self.0.as_path().display()
		))
	}
}

#[derive(Debug)]
pub struct CannotRedeclareVariable<'a> {
	pub name: &'a str,
}

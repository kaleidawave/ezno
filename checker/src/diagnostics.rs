//! Contains type checking errors, warnings and related structures

use serde::Serialize;
use source_map::{SourceId, Span};
use std::{
	fmt::{self, Debug, Display},
	iter,
	path::PathBuf,
};

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum DiagnosticKind {
	Error,
	Warning,
	Info,
}

/// Contains information
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum Diagnostic {
	/// Does not have positional information
	Global {
		reason: String,
		kind: DiagnosticKind,
	},
	Position {
		reason: String,
		position: Span,
		kind: DiagnosticKind,
	},
	PositionWithAdditionLabels {
		reason: String,
		position: Span,
		labels: Vec<(String, Option<Span>)>,
		kind: DiagnosticKind,
	},
}

impl Diagnostic {
	pub fn sources<'a>(&'a self) -> impl Iterator<Item = SourceId> + 'a {
		use either::{Left, Right};
		match self {
			Diagnostic::Global { .. } => Left(Left(iter::empty())),
			Diagnostic::Position { position: span, .. } => Left(Right(iter::once(span.source_id))),
			Diagnostic::PositionWithAdditionLabels { position: pos, labels, .. } => {
				Right(iter::once(pos.source_id).chain(
					labels.iter().flat_map(|(_, span)| span.as_ref().map(|span| span.source_id)),
				))
			}
		}
	}

	pub fn reason(&self) -> &str {
		match self {
			Diagnostic::Global { reason, .. }
			| Diagnostic::Position { reason, .. }
			| Diagnostic::PositionWithAdditionLabels { reason, .. } => &reason,
		}
	}
}

#[derive(Default, serde::Serialize)]
#[serde(transparent)]
pub struct DiagnosticsContainer {
	diagnostics: Vec<Diagnostic>,
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
		self.diagnostics.push(error.into())
	}

	pub fn add_warning<T: Into<Diagnostic>>(&mut self, warning: T) {
		self.diagnostics.push(warning.into())
	}

	pub fn add_info<T: Into<Diagnostic>>(&mut self, info: T) {
		self.diagnostics.push(info.into())
	}

	pub fn has_error(&self) -> bool {
		self.has_error
	}

	pub fn sources<'a>(&'a self) -> impl Iterator<Item = SourceId> + 'a {
		self.diagnostics.iter().flat_map(|item| item.sources())
	}

	pub fn into_iter(self) -> impl DoubleEndedIterator<Item = Diagnostic> {
		self.diagnostics.into_iter()
	}

	#[doc(hidden)]
	pub fn get_diagnostics(self) -> Vec<Diagnostic> {
		self.diagnostics
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
	context::GeneralContext,
	types::{printing::print_type, TypeId, TypeStore},
};

/// TODO could be more things, for instance a property missing etc
pub enum TypeStringRepresentation {
	Type(String),
}

impl TypeStringRepresentation {
	pub fn from_type_id(
		id: TypeId,
		env: &GeneralContext,
		types: &TypeStore,
		debug_mode: bool,
	) -> Self {
		if debug_mode {
			todo!()
		// 	let ty = get_on_ctx!(env.get_type_by_id(id));
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
		Diagnostic::Global { reason: "No environment".to_owned(), kind: DiagnosticKind::Error }
	}
}

// Contains known internal errors and warnings
// Contained here in a module to separate user facing
mod defined_errors_and_warnings {
	use crate::context::AssignmentError;
	use source_map::Span;

	use crate::{
		events::FunctionCallingError,
		structures::{self, operators::UnaryOperator},
		Diagnostic,
	};
	use std::path;

	use super::TypeStringRepresentation;

	/// Covers multiplication, subtraction, modulo etc
	/// TODO something better?
	pub struct InvalidMathematicalOperation {
		pub(crate) lhs: TypeStringRepresentation,
		pub(crate) rhs: TypeStringRepresentation,
		pub(crate) operator: structures::operators::BinaryOperator,
		pub(crate) position: Span,
	}

	/// Reasons for errors, intermediate type for generating [Diagnostic]s
	/// e.g. cannot Call, cannot equate, duplicate key etc
	pub(crate) enum TypeCheckError<'a> {
		FunctionCallingError(FunctionCallingError),
		PropertyDoesNotExist {
			property: TypeStringRepresentation,
			on: TypeStringRepresentation,
			site: Span,
		},
		RestParameterAnnotationShouldBeArrayType(Span),
		/// TODO better name
		NonExistentType(String),
		CouldNotFindVariable {
			variable: &'a str,
			possibles: Vec<&'a str>,
			position: Span,
		},
		CouldNotFindType(&'a str, Span),
		TypeHasNoGenericParameters(String, Span),
		AssignmentError(AssignmentError),
		InvalidComparison(TypeStringRepresentation, TypeStringRepresentation),
		InvalidAddition(TypeStringRepresentation, TypeStringRepresentation),
		InvalidMathematicalOperation(InvalidMathematicalOperation),
		InvalidBitwiseOperation(
			TypeStringRepresentation,
			TypeStringRepresentation,
			structures::operators::BitwiseOperators,
		),
		InvalidUnaryOperation(UnaryOperator, TypeStringRepresentation),
		ReturnedTypeDoesNotMatch {
			expected_return_type: TypeStringRepresentation,
			returned_type: TypeStringRepresentation,
			position: Span,
		},
		// TODO are these the same errors?
		TypeIsNotIndexable(TypeStringRepresentation),
		TypeIsNotIterable(TypeStringRepresentation),
		// This could be a syntax error but that is difficult to type...
		NonTopLevelExport,
		// TODO implies the presence of, which isn't always true
		FieldNotExported(&'a str, &'a path::Path, Span),
		InvalidJSXAttribute {
			attribute_name: String,
			attribute_type: TypeStringRepresentation,
			value_type: TypeStringRepresentation,
			// TODO
			attribute_type_site: (),
			value_site: Span,
		},
		InvalidJSXInterpolatedValue {
			interpolation_site: Span,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		/// for the `satisfies` keyword
		NotSatisfied {
			at: Span,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		Unsupported {
			thing: &'static str,
			at: Span,
		},
		ReDeclaredVariable {
			name: &'a str,
			pos: Span,
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
						position,
						kind: super::DiagnosticKind::Error,
					}
				}
				TypeCheckError::CouldNotFindType(reference, pos) => Diagnostic::Position {
					reason: format!("Could not find type '{}'", reference),
					position: pos,
					kind: super::DiagnosticKind::Error,
				},
				TypeCheckError::PropertyDoesNotExist { property, on, site } => {
					Diagnostic::Position {
						reason: format!("No property with {} on {}", property, on),
						position: site,
						kind: super::DiagnosticKind::Error,
					}
				}
				TypeCheckError::FunctionCallingError(error) => match error {
					FunctionCallingError::InvalidArgumentType {
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
								position: argument_position,
								labels: vec![(
									format!(
										"Parameter {} was specialized with type {}",
										parameter_type, restriction
									),
									Some(restriction_pos),
								)],
								kind: super::DiagnosticKind::Error,
							}
						} else {
							Diagnostic::PositionWithAdditionLabels {
								reason: format!(
									"Argument of type {} is not assignable to {}",
									argument_type, parameter_type
								),
								position: argument_position,
								labels: vec![(
									format!("Parameter has type {}", parameter_type),
									Some(parameter_position),
								)],
								kind: super::DiagnosticKind::Error,
							}
						}
					}
					FunctionCallingError::MissingArgument { parameter_pos } => todo!(),
					FunctionCallingError::ExtraArguments { count, position } => todo!(),
					FunctionCallingError::NotCallable { calling } => todo!(),
					//  Diagnostic::Position {
					// 	reason: format!("Cannot call {}", calling),
					// 	position: at,
					// 	kind: super::DiagnosticKind::Error,
					// },
					FunctionCallingError::ReferenceRestrictionDoesNotMatch {
						reference,
						requirement,
						found,
					} => todo!(),
					// Diagnostic::Position {
					// 	reason: format!(
					// 		"Calling function requires {} to be {}, found {}",
					// 		identifier, requirement, found
					// 	),
					// 	position: call_site,
					// 	kind: super::DiagnosticKind::Error,
					// },
				},
				//  => ,
				TypeCheckError::AssignmentError(error) => match error {
					AssignmentError::InvalidDeclaration {
						variable_type,
						variable_site,
						value_type,
						value_site,
					} => Diagnostic::PositionWithAdditionLabels {
						reason: format!(
							"Type {} is not assignable to type {}",
							value_type, variable_type
						),
						position: value_site,
						labels: vec![(
							format!("Variable declared with type {}", variable_type),
							Some(variable_site),
						)],
						kind: super::DiagnosticKind::Error,
					},
					_ => todo!(),
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
					position: value_site,
					kind: super::DiagnosticKind::Error,
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
					position,
					kind: super::DiagnosticKind::Error,
				},
				// TypeCheckError::MissingArguments { function, parameter_pos, call_site } => {
				// Diagnostic::PositionWithAdditionLabels {
				// 		reason: format!("Calling {}, found missing arguments", function),
				// 		position: call_site,
				// 		labels: vec![("Parameter defined here".to_owned(), Some(parameter_pos))],
				// 		kind: super::DiagnosticKind::Error,
				// 	}
				// }
				TypeCheckError::NonExistentType(_) => todo!(),
				TypeCheckError::TypeHasNoGenericParameters(_, _) => todo!(),
				// TypeCheckError::CannotAssignToConstant {
				// 	variable_name,
				// 	variable_position,
				// 	assignment_position,
				// } => Diagnostic::PositionWithAdditionLabels {
				// 	reason: format!("Cannot reassign to constant variable '{}'", variable_name),
				// 	position: assignment_position,
				// 	labels: vec![(
				// 		"Constant variable defined here".to_owned(),
				// 		Some(variable_position),
				// 	)],
				// 	kind: super::DiagnosticKind::Error,
				// },
				// TypeCheckError::CannotAssignToFrozen { variable_type, assignment_position } => {
				// 	Diagnostic::Position {
				// 		reason: format!("{} is frozen", variable_type),
				// 		position: assignment_position,
				// 		kind: super::DiagnosticKind::Error,
				// 	}
				// }
				TypeCheckError::InvalidComparison(_, _) => todo!(),
				TypeCheckError::InvalidAddition(_, _) => todo!(),
				TypeCheckError::InvalidMathematicalOperation(InvalidMathematicalOperation {
					lhs,
					rhs,
					operator,
					position,
				}) => Diagnostic::Position {
					reason: format!("Cannot {:?} {} and {}", operator, lhs, rhs),
					position,
					kind: super::DiagnosticKind::Error,
				},
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
						position: pos,
						kind: super::DiagnosticKind::Error,
					}
				}
				TypeCheckError::Unsupported { thing, at } => Diagnostic::Position {
					reason: format!("Unsupported: {}", thing),
					position: at,
					kind: super::DiagnosticKind::Error,
				},
				// TypeCheckError::NotWritable { pos } => Diagnostic::Position {
				// 	reason: format!("Cannot assign to immutable property"),
				// 	position: pos,
				// 	kind: super::DiagnosticKind::Error,
				// },
				// TypeCheckError::CannotAssign { pos, value_pos, constraint, to } => {
				// 	Diagnostic::PositionWithAdditionLabels {
				// 		reason: format!("Cannot assign {} to property of type {}", to, constraint),
				// 		position: pos,
				// 		labels: vec![(format!("Expression has type {to}"), Some(value_pos))],
				// 		kind: super::DiagnosticKind::Error,
				// 	}
				// }
				// TypeCheckError::ReDeclaredVariable { name, pos } => Diagnostic::Position {
				// 	reason: format!("Cannot redeclare {} in scope", name),
				// 	position: pos,
				// 	kind: super::DiagnosticKind::Error,
				// },
				TypeCheckError::ReDeclaredVariable { name, pos } => todo!(),
				TypeCheckError::FunctionDoesNotMeetConstraint {
					function_constraint,
					function_type,
					position,
				} => Diagnostic::Position {
					reason: format!(
						"{} constraint on function does not match synthesized form {}",
						function_constraint, function_type
					),
					position,
					kind: super::DiagnosticKind::Error,
				},
				TypeCheckError::StatementsNotRun { between } => Diagnostic::Position {
					reason: "Statements are never run".to_owned(),
					position: between,
					kind: super::DiagnosticKind::Error,
				},
				TypeCheckError::NotSatisfied { at, expected, found } => Diagnostic::Position {
					reason: format!("Expected {} found {}", expected, found),
					position: at,
					kind: super::DiagnosticKind::Error,
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
		Unimplemented {
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
					position: span,
					kind: super::DiagnosticKind::Warning,
				},
				TypeCheckWarning::DeadBranch { expression_span, expression_value } => {
					Diagnostic::Position {
						reason: format!("Expression is always {:?}", expression_value),
						position: expression_span,
						kind: super::DiagnosticKind::Warning,
					}
				}
				TypeCheckWarning::IgnoringAsExpression(span) => Diagnostic::Position {
					reason: "'as' expressions are ignore by the checker".to_owned(),
					position: span,
					kind: super::DiagnosticKind::Warning,
				},
				TypeCheckWarning::Unimplemented { thing, at } => Diagnostic::Position {
					reason: format!("Unsupported: {}", thing),
					position: at,
					kind: super::DiagnosticKind::Warning,
				},
				TypeCheckWarning::UselessExpression { expression_span } => Diagnostic::Position {
					reason: "Expression is always true".to_owned(),
					position: expression_span,
					kind: super::DiagnosticKind::Warning,
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
		Diagnostic::Global {
			reason: format!(
				"Could not find type definition module at '{}'",
				self.0.as_path().display()
			),
			kind: DiagnosticKind::Error,
		}
	}
}

pub(crate) struct EntryPointNotFound(pub PathBuf);

impl Into<Diagnostic> for EntryPointNotFound {
	fn into(self) -> Diagnostic {
		Diagnostic::Global {
			reason: format!("Could not entry point module at '{}'", self.0.as_path().display()),
			kind: DiagnosticKind::Error,
		}
	}
}

#[derive(Debug)]
pub struct CannotRedeclareVariable<'a> {
	pub name: &'a str,
}

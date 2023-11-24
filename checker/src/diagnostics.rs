//! Contains type checking errors, warnings and related structures

use serde::Serialize;
use source_map::{SourceId, Span, SpanWithSource};
use std::{
	fmt::{self, Debug, Display},
	iter,
	path::PathBuf,
};

#[derive(Serialize, Debug)]
pub enum DiagnosticKind {
	Error,
	Warning,
	Info,
}

/// Contains information
#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum Diagnostic {
	/// Does not have positional information
	Global {
		reason: String,
		kind: DiagnosticKind,
	},
	Position {
		reason: String,
		position: SpanWithSource,
		kind: DiagnosticKind,
	},
	PositionWithAdditionLabels {
		reason: String,
		position: SpanWithSource,
		labels: Vec<(String, Option<SpanWithSource>)>,
		kind: DiagnosticKind,
	},
}

impl Diagnostic {
	pub fn sources(&self) -> impl Iterator<Item = SourceId> + '_ {
		use either::{Left, Right};
		match self {
			Diagnostic::Global { .. } => Left(Left(iter::empty())),
			Diagnostic::Position { position: span, .. } => Left(Right(iter::once(span.source))),
			Diagnostic::PositionWithAdditionLabels { position: pos, labels, .. } => {
				Right(iter::once(pos.source).chain(
					labels.iter().flat_map(|(_, span)| span.as_ref().map(|span| span.source)),
				))
			}
		}
	}

	pub fn reason(&self) -> &str {
		match self {
			Diagnostic::Global { reason, .. }
			| Diagnostic::Position { reason, .. }
			| Diagnostic::PositionWithAdditionLabels { reason, .. } => reason,
		}
	}

	pub fn reason_and_position(self) -> (String, Option<SpanWithSource>) {
		match self {
			Diagnostic::Global { reason, .. } => (reason, None),
			Diagnostic::Position { reason, position, .. }
			| Diagnostic::PositionWithAdditionLabels { reason, position, .. } => (reason, Some(position)),
		}
	}
}

/// TODO this is one variant, others should pipe strait to stdout or put it on a channel etc
#[derive(Default, serde::Serialize)]
#[serde(transparent)]
pub struct DiagnosticsContainer {
	diagnostics: Vec<Diagnostic>,
	// Quick way to check whether a error was added
	#[serde(skip_serializing)]
	has_error: bool,
}

// TODO the add methods are the same...
impl DiagnosticsContainer {
	pub fn new() -> Self {
		Self { diagnostics: Default::default(), has_error: false }
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

	pub fn sources(&self) -> impl Iterator<Item = SourceId> + '_ {
		self.diagnostics.iter().flat_map(|item| item.sources())
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

impl IntoIterator for DiagnosticsContainer {
	type Item = Diagnostic;

	type IntoIter = std::vec::IntoIter<Diagnostic>;

	fn into_iter(self) -> Self::IntoIter {
		self.diagnostics.into_iter()
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

pub enum PropertyRepresentation {
	Type(String),
	StringKey(String),
}

impl TypeStringRepresentation {
	pub fn from_type_id(
		id: TypeId,
		ctx: &GeneralContext,
		types: &TypeStore,
		debug_mode: bool,
	) -> Self {
		let value = print_type(id, types, ctx, debug_mode);
		Self::Type(value)
	}
}

impl Display for TypeStringRepresentation {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeStringRepresentation::Type(ty) => f.write_str(ty),
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
	use crate::{behavior, context::AssignmentError, types::calling::FunctionCallingError};
	use source_map::{Span, SpanWithSource};

	use crate::Diagnostic;
	use std::path;

	use super::{PropertyRepresentation, TypeStringRepresentation};

	/// Reasons for errors, intermediate type for generating [Diagnostic]s
	/// e.g. cannot Call, cannot equate, duplicate key etc
	pub(crate) enum TypeCheckError<'a> {
		FunctionCallingError(FunctionCallingError),
		PropertyDoesNotExist {
			on: TypeStringRepresentation,
			property: PropertyRepresentation,
			site: SpanWithSource,
		},
		RestParameterAnnotationShouldBeArrayType(SpanWithSource),
		CouldNotFindVariable {
			variable: &'a str,
			possibles: Vec<&'a str>,
			position: SpanWithSource,
		},
		CouldNotFindType(&'a str, SpanWithSource),
		TypeHasNoGenericParameters(String, SpanWithSource),
		AssignmentError(AssignmentError),
		InvalidComparison(TypeStringRepresentation, TypeStringRepresentation),
		InvalidAddition(TypeStringRepresentation, TypeStringRepresentation),
		InvalidUnaryOperation(crate::behavior::operations::PureUnary, TypeStringRepresentation),
		ReturnedTypeDoesNotMatch {
			expected_return_type: TypeStringRepresentation,
			returned_type: TypeStringRepresentation,
			annotation_position: SpanWithSource,
			returned_position: SpanWithSource,
		},
		// TODO are these the same errors?
		TypeIsNotIndexable(TypeStringRepresentation),
		TypeIsNotIterable(TypeStringRepresentation),
		// This could be a syntax error but that is difficult to type...
		NonTopLevelExport(SpanWithSource),
		FieldNotExported {
			file: &'a str,
			importing: &'a str,
			position: SpanWithSource,
		},
		InvalidJSXAttribute {
			attribute_name: String,
			attribute_type: TypeStringRepresentation,
			value_type: TypeStringRepresentation,
			// TODO
			attribute_type_site: (),
			value_site: SpanWithSource,
		},
		InvalidJSXInterpolatedValue {
			interpolation_site: SpanWithSource,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		/// for the `satisfies` keyword
		NotSatisfied {
			at: SpanWithSource,
			expected: TypeStringRepresentation,
			found: TypeStringRepresentation,
		},
		Unsupported {
			thing: &'static str,
			at: SpanWithSource,
		},
		ReDeclaredVariable {
			name: &'a str,
			position: SpanWithSource,
		},
		/// TODO temp, needs more info
		FunctionDoesNotMeetConstraint {
			function_constraint: TypeStringRepresentation,
			function_type: TypeStringRepresentation,
			position: SpanWithSource,
		},
		StatementsNotRun {
			between: SpanWithSource,
		},
		CannotRedeclareVariable {
			name: String,
			position: SpanWithSource,
		},
		// TODO parameter position
		GenericArgumentDoesNotMeetRestriction {
			parameter_restriction: TypeStringRepresentation,
			argument: TypeStringRepresentation,
			position: SpanWithSource,
		},
		NotDefinedOperator(&'static str, SpanWithSource),
		PropertyNotWriteable(SpanWithSource),
		NotTopLevelImport(SpanWithSource),
		DoubleDefaultExport(SpanWithSource),
		CannotOpenFile {
			file: crate::CouldNotOpenFile,
			position: Option<SpanWithSource>,
		},
		VariableNotDefinedInContext {
			variable: &'a str,
			expected_context: &'a str,
			current_context: String,
			position: SpanWithSource,
		},
		TypeNeedsTypeArguments(&'a str, SpanWithSource),
		CannotFindType(&'a str, SpanWithSource),
		TypeAliasAlreadyDeclared {
			name: String,
			position: SpanWithSource,
		},
	}

	impl From<TypeCheckError<'_>> for Diagnostic {
		fn from(error: TypeCheckError<'_>) -> Self {
			let kind = super::DiagnosticKind::Error;
			match error {
				TypeCheckError::CouldNotFindVariable { variable, possibles, position } => {
					Diagnostic::Position {
						reason: format!(
							"Could not find variable {} in scope",
							variable,
							// possibles Consider '{:?}'
						),
						position,
						kind,
					}
				}
				TypeCheckError::CouldNotFindType(reference, pos) => Diagnostic::Position {
					reason: format!("Could not find type '{}'", reference),
					position: pos,
					kind,
				},
				TypeCheckError::PropertyDoesNotExist { property, on, site } => {
					Diagnostic::Position {
						reason: match property {
							PropertyRepresentation::Type(ty) => format!("No property of type {} on {}", ty, on),
							PropertyRepresentation::StringKey(property) => format!("No property '{}' on {}", property, on),
						},
						position: site,
						kind,
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
									"Argument of type {} is not assignable to parameter of type {}",
									argument_type, restriction
								),
								position: argument_position,
								labels: vec![(
									format!(
										"{} was specialised with type {}",
										parameter_type, restriction
									),
									Some(restriction_pos),
								)],
								kind,
							}
						} else {
							Diagnostic::PositionWithAdditionLabels {
								reason: format!(
									"Argument of type {} is not assignable to parameter of type {}",
									argument_type, parameter_type
								),
								position: argument_position,
								labels: vec![(
									format!("Parameter has type {}", parameter_type),
									Some(parameter_position),
								)],
								kind,
							}
						}
					}
					FunctionCallingError::MissingArgument { parameter_position, call_site } => {
						Diagnostic::PositionWithAdditionLabels {
							reason: "Missing argument".into(),
							position: call_site,
							kind,
							labels: vec![(
								"(non-optional) Parameter declared here".into(),
								Some(parameter_position),
							)],
						}
					}
					FunctionCallingError::ExcessArguments { count, position } => {
						Diagnostic::Position {
							reason: "Excess argument".into(),
							position,
							kind,
						}
					}
					FunctionCallingError::NotCallable { calling, call_site } => {
						Diagnostic::Position {
							reason: format!("Cannot call type {calling}"),
							position: call_site,
							kind,
						}
					}
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
					// 	kind,
					// },
					FunctionCallingError::CyclicRecursion(_, call_site) => Diagnostic::Position {
						reason: "Encountered recursion".into(),
						position: call_site,
						kind,
					},
					FunctionCallingError::NoLogicForIdentifier(name, position) => Diagnostic::Position { reason: format!("no logic for constant function {name}"), kind, position },
					FunctionCallingError::NeedsToBeCalledWithNewKeyword(position) => Diagnostic::Position { reason: "class constructor must be called with new".to_owned(), kind, position },
				},
				//  => ,
				TypeCheckError::AssignmentError(error) => match error {
					AssignmentError::DoesNotMeetConstraint {
						variable_type,
						variable_site,
						value_type,
						value_site,
					} => Diagnostic::PositionWithAdditionLabels {
						reason: format!(
							"Type {value_type} is not assignable to type {variable_type}",
						),
						position: value_site,
						labels: vec![(
							format!("Variable declared with type {variable_type}"),
							Some(variable_site),
						)],
						kind,
					},
					AssignmentError::PropertyConstraint {
						property_type,
						value_type,
						assignment_position,
					} => Diagnostic::Position {
						reason: format!(
							"Type {value_type} does not meet property constraint {property_type}"
						),
						position: assignment_position,
						kind,
					},
					AssignmentError::Constant(position) => Diagnostic::Position {
						reason: "Cannot assign to constant".into(),
						position,
						kind,
					},
					AssignmentError::VariableNotFound { variable, assignment_position } => {
						Diagnostic::Position {
							reason: format!("Cannot assign to unknown variable {variable}"),
							position: assignment_position,
							kind,
						}
					}
				},
				TypeCheckError::InvalidJSXAttribute {
					attribute_name,
					attribute_type,
					value_type,
					attribute_type_site: variable_site,
					value_site,
				} => Diagnostic::Position {
					reason: format!(
						"Type {attribute_name} is not assignable to {value_type} attribute of type {attribute_type}",
					),
					position: value_site,
					kind,
				},
				TypeCheckError::ReturnedTypeDoesNotMatch {
					annotation_position,
					returned_position,
					expected_return_type,
					returned_type,
				} => Diagnostic::PositionWithAdditionLabels {
					reason: format!(
						"Cannot return {returned_type} because the function is expected to return {expected_return_type}",
					),
					labels: vec![(
						format!("Function annotated to return {expected_return_type} here"),
						Some(annotation_position.clone()),
					)],
					position: returned_position,
					kind,
				},
				TypeCheckError::TypeHasNoGenericParameters(name, position) => {
					Diagnostic::Position {
						reason: format!("Type '{name}' has no generic parameters",),
						position,
						kind,
					}
				}
				TypeCheckError::InvalidComparison(_, _) => todo!(),
				TypeCheckError::InvalidAddition(_, _) => todo!(),
				TypeCheckError::InvalidUnaryOperation(_, _) => todo!(),
				TypeCheckError::TypeIsNotIndexable(_) => todo!(),
				TypeCheckError::TypeIsNotIterable(_) => todo!(),
				TypeCheckError::NonTopLevelExport(position) => Diagnostic::Position {
					reason: "Cannot export at not top level".to_owned(),
					position,
					kind,
				},
				TypeCheckError::FieldNotExported { file, importing, position } => {
					Diagnostic::Position {
						reason: format!("{importing} not exported from {file}"),
						position,
						kind,
					}
				}
				TypeCheckError::InvalidJSXInterpolatedValue {
					interpolation_site,
					expected,
					found,
				} => todo!(),
				TypeCheckError::RestParameterAnnotationShouldBeArrayType(pos) => {
					Diagnostic::Position {
						reason: "Rest parameter annotation should be array type".to_owned(),
						position: pos,
						kind,
					}
				}
				TypeCheckError::Unsupported { thing, at } => Diagnostic::Position {
					reason: format!("Unsupported: {}", thing),
					position: at,
					kind,
				},
				TypeCheckError::ReDeclaredVariable { name, position: pos } => todo!(),
				TypeCheckError::FunctionDoesNotMeetConstraint {
					function_constraint,
					function_type,
					position,
				} => Diagnostic::Position {
					reason: format!(
						"{} constraint on function does not match synthesised form {}",
						function_constraint, function_type
					),
					position,
					kind,
				},
				TypeCheckError::StatementsNotRun { between } => Diagnostic::Position {
					reason: "Statements are never run".to_owned(),
					position: between,
					kind,
				},
				TypeCheckError::NotSatisfied { at, expected, found } => Diagnostic::Position {
					reason: format!("Expected {expected}, found {found}"),
					position: at,
					kind,
				},
				TypeCheckError::CannotRedeclareVariable { name, position } => {
					Diagnostic::Position {
						reason: format!("Cannot redeclare variable {name}"),
						position,
						kind,
					}
				}
				TypeCheckError::NotDefinedOperator(op, position) => Diagnostic::Position {
					reason: format!("Operator not typed {op}"),
					position,
					kind,
				},
				TypeCheckError::PropertyNotWriteable(position) => Diagnostic::Position {
					reason: "property not writeable".into(),
					position,
					kind,
				},
				TypeCheckError::GenericArgumentDoesNotMeetRestriction {
					argument,
					parameter_restriction,
					position,
				} => Diagnostic::Position {
					reason: format!(
						"Generic argument {argument} does not match {parameter_restriction}"
					),
					position,
					kind,
				},
				TypeCheckError::NotTopLevelImport(position) => Diagnostic::Position {
					reason: "Import must be in the top of the scope".to_owned(),
					position,
					kind,
				},
				TypeCheckError::DoubleDefaultExport(_) => todo!(),
				TypeCheckError::CannotOpenFile { file, position } => if let Some(position) = position {
					Diagnostic::Position {
						reason: "Cannot find file".to_owned(),
						position,
						kind,
					}
				} else {
					Diagnostic::Global { reason: format!("Cannot find file {}", file.0.display()), kind }
				},
				TypeCheckError::VariableNotDefinedInContext {
					variable,
					expected_context,
					current_context,
					position,
				} => Diagnostic::Position {
					reason: format!("{variable} is only available on the {expected_context}, currently in {current_context}"),
					position,
					kind,
				},
				TypeCheckError::TypeNeedsTypeArguments(ty, position) => Diagnostic::Position {
					reason: format!("Type {ty} requires type arguments"),
					position,
					kind,
				},
				TypeCheckError::CannotFindType(ty, position) => Diagnostic::Position {
					reason: format!("Cannot find type {ty}"),
					position,
					kind,
				},
				TypeCheckError::TypeAliasAlreadyDeclared { name, position } => Diagnostic::Position {
					reason: format!("Type {name} already declared"),
					position,
					kind,
				},
			}
		}
	}

	pub enum TypeCheckWarning {
		AwaitUsedOnNonPromise(SpanWithSource),
		/// TODO could be an error at some point
		DeadBranch {
			expression_span: SpanWithSource,
			expression_value: bool,
		},
		IgnoringAsExpression(SpanWithSource),
		Unimplemented {
			thing: &'static str,
			at: SpanWithSource,
		},
		UselessExpression {
			expression_span: SpanWithSource,
			// TODO other branch information
		},
	}

	impl From<TypeCheckWarning> for Diagnostic {
		fn from(val: TypeCheckWarning) -> Self {
			let kind = super::DiagnosticKind::Warning;

			match val {
				TypeCheckWarning::AwaitUsedOnNonPromise(position) => Diagnostic::Position {
					reason: "Unnecessary await expression / type is not promise".to_owned(),
					position,
					kind,
				},
				TypeCheckWarning::DeadBranch { expression_span, expression_value } => {
					Diagnostic::Position {
						reason: format!("Expression is always {:?}", expression_value),
						position: expression_span,
						kind,
					}
				}
				TypeCheckWarning::IgnoringAsExpression(position) => Diagnostic::Position {
					reason: "'as' expressions are ignore by the checker".to_owned(),
					position,
					kind,
				},
				TypeCheckWarning::Unimplemented { thing, at } => Diagnostic::Position {
					reason: format!("Unsupported: {}", thing),
					position: at,
					kind,
				},
				TypeCheckWarning::UselessExpression { expression_span } => Diagnostic::Position {
					reason: "Expression is always true".to_owned(),
					position: expression_span,
					kind,
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

impl From<TypeDefinitionModuleNotFound> for Diagnostic {
	fn from(val: TypeDefinitionModuleNotFound) -> Self {
		Diagnostic::Global {
			reason: format!(
				"Could not find type definition module at '{}'",
				val.0.as_path().display()
			),
			kind: DiagnosticKind::Error,
		}
	}
}

pub(crate) struct EntryPointNotFound(pub PathBuf);

impl From<EntryPointNotFound> for Diagnostic {
	fn from(val: EntryPointNotFound) -> Self {
		Diagnostic::Global {
			reason: format!("Could not entry point module at '{}'", val.0.as_path().display()),
			kind: DiagnosticKind::Error,
		}
	}
}

#[derive(Debug)]
pub struct CannotRedeclareVariable<'a> {
	pub name: &'a str,
}

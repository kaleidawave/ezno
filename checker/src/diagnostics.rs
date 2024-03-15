//! Contains type checking errors, warnings and related structures

#![allow(clippy::upper_case_acronyms)]

use crate::{
	context::{environment::Label, information::InformationChain},
	diagnostics,
	types::{printing::print_type_with_type_arguments, GenericChain},
};
use source_map::{SourceId, SpanWithSource};
use std::{
	fmt::{self, Debug, Display},
	iter,
	path::PathBuf,
};

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize), serde(rename_all = "lowercase"))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub enum DiagnosticKind {
	Error,
	Warning,
	Info,
}

/// Contains information
#[derive(Debug)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize), serde(untagged))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
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
	PositionWithAdditionalLabels {
		reason: String,
		position: SpanWithSource,
		labels: Vec<(String, Option<SpanWithSource>)>,
		kind: DiagnosticKind,
	},
}

/// Temporary dead zone. Between the variable identifier being hoisted and the value being assigned
pub struct TDZ {
	pub variable_name: String,
	pub position: SpanWithSource,
}

pub struct NotInLoopOrCouldNotFindLabel {
	pub label: Label,
	pub position: SpanWithSource,
}

impl Diagnostic {
	pub fn sources(&self) -> impl Iterator<Item = SourceId> + '_ {
		use either::{Left, Right};
		match self {
			Diagnostic::Global { .. } => Left(Left(iter::empty())),
			Diagnostic::Position { position: span, .. } => Left(Right(iter::once(span.source))),
			Diagnostic::PositionWithAdditionalLabels { position: pos, labels, .. } => {
				Right(iter::once(pos.source).chain(
					labels.iter().filter_map(|(_, span)| span.as_ref().map(|span| span.source)),
				))
			}
		}
	}

	#[must_use]
	pub fn reason(&self) -> &str {
		match self {
			Diagnostic::Global { reason, .. }
			| Diagnostic::Position { reason, .. }
			| Diagnostic::PositionWithAdditionalLabels { reason, .. } => reason,
		}
	}

	#[must_use]
	pub fn reason_and_position(self) -> (String, Option<SpanWithSource>) {
		match self {
			Diagnostic::Global { reason, .. } => (reason, None),
			Diagnostic::Position { reason, position, .. }
			| Diagnostic::PositionWithAdditionalLabels { reason, position, .. } => (reason, Some(position)),
		}
	}

	#[must_use]
	pub fn kind(&self) -> DiagnosticKind {
		match self {
			Diagnostic::Global { kind, .. }
			| Diagnostic::Position { kind, .. }
			| Diagnostic::PositionWithAdditionalLabels { kind, .. } => *kind,
		}
	}
}

/// TODO this is one variant, others should pipe strait to stdout or put it on a channel etc
#[derive(Default)]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize), serde(transparent))]
#[cfg_attr(target_family = "wasm", derive(tsify::Tsify))]
pub struct DiagnosticsContainer {
	diagnostics: Vec<Diagnostic>,
	// Quick way to check whether a error was added
	#[cfg_attr(feature = "serde-serialize", serde(skip_serializing))]
	has_error: bool,
}

// TODO the add methods are the same...
impl DiagnosticsContainer {
	#[must_use]
	pub fn new() -> Self {
		Self { diagnostics: Default::default(), has_error: false }
	}

	pub fn add_error<T: Into<Diagnostic>>(&mut self, error: T) {
		self.has_error = true;
		self.diagnostics.push(error.into());
	}

	pub fn add_warning<T: Into<Diagnostic>>(&mut self, warning: T) {
		self.diagnostics.push(warning.into());
	}

	pub fn add_info<T: Into<Diagnostic>>(&mut self, info: T) {
		self.diagnostics.push(info.into());
	}

	#[must_use]
	pub fn has_error(&self) -> bool {
		self.has_error
	}

	pub fn sources(&self) -> impl Iterator<Item = SourceId> + '_ {
		self.diagnostics.iter().flat_map(diagnostics::Diagnostic::sources)
	}

	#[doc(hidden)]
	#[must_use]
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

use crate::types::{printing::print_type, TypeId, TypeStore};

/// TODO could be more things, for instance a property missing etc
pub enum TypeStringRepresentation {
	Type(String),
}

pub enum PropertyRepresentation {
	Type(String),
	StringKey(String),
}

impl TypeStringRepresentation {
	#[must_use]
	pub fn from_type_id(
		id: TypeId,
		ctx: &impl InformationChain,
		types: &TypeStore,
		debug_mode: bool,
	) -> Self {
		let value = print_type(id, types, ctx, debug_mode);
		Self::Type(value)
	}

	#[must_use]
	pub fn from_type_id_with_generics(
		id: TypeId,
		type_arguments: Option<GenericChain>,
		ctx: &impl InformationChain,
		types: &TypeStore,
		debug_mode: bool,
	) -> Self {
		let value = print_type_with_type_arguments(id, type_arguments, types, ctx, debug_mode);
		Self::Type(value)
	}

	/// TODO working it out
	pub(crate) fn from_property_constraint(
		property_constraint: crate::context::Logical<crate::PropertyValue>,
		generics: Option<GenericChain>,
		ctx: &impl InformationChain,
		types: &TypeStore,
		debug_mode: bool,
	) -> TypeStringRepresentation {
		match property_constraint {
			crate::context::Logical::Pure(constraint) => match constraint {
				crate::PropertyValue::Value(v) => {
					let value = print_type_with_type_arguments(v, generics, types, ctx, debug_mode);
					Self::Type(value)
				}
				crate::PropertyValue::Getter(_) => todo!(),
				crate::PropertyValue::Setter(_) => todo!(),
				crate::PropertyValue::Deleted => todo!(),
				crate::PropertyValue::Dependent { .. } => todo!(),
			},
			crate::context::Logical::Or { .. } => {
				todo!()
				// let left = Self::from_property_constraint(*left, None, ctx, types, debug_mode);
				// let right = Self::from_property_constraint(*right, None, ctx, types, debug_mode);

				// #[allow(irrefutable_let_patterns)]
				// if let (TypeStringRepresentation::Type(mut l), TypeStringRepresentation::Type(r)) =
				// 	(left, right)
				// {
				// 	l.push_str(&r);
				// 	TypeStringRepresentation::Type(l)
				// } else {
				// 	unreachable!()
				// }
			}
			crate::context::Logical::Implies { on, antecedent } => {
				if generics.is_some() {
					todo!("chaining")
				}
				Self::from_property_constraint(
					*on,
					Some(GenericChain::new(&antecedent)),
					ctx,
					types,
					debug_mode,
				)
			}
		}
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
	fn from(_error: NoEnvironmentSpecified) -> Self {
		Diagnostic::Global { reason: "No environment".to_owned(), kind: DiagnosticKind::Error }
	}
}

// Contains known internal errors and warnings
// Contained here in a module to separate user facing
mod defined_errors_and_warnings {
	use crate::{
		context::AssignmentError,
		features::{modules::CouldNotOpenFile, operations::MathematicalAndBitwise},
		types::calling::FunctionCallingError,
	};
	use source_map::SpanWithSource;

	use crate::Diagnostic;

	use super::{
		NotInLoopOrCouldNotFindLabel, PropertyRepresentation, TypeStringRepresentation, TDZ,
	};

	/// Reasons for errors, intermediate type for generating [Diagnostic]s
	/// e.g. cannot Call, cannot equate, duplicate key etc
	#[allow(unused)]
	pub(crate) enum TypeCheckError<'a> {
		FunctionCallingError(FunctionCallingError),
		PropertyDoesNotExist {
			on: TypeStringRepresentation,
			property: PropertyRepresentation,
			site: SpanWithSource,
		},
		NotInLoopOrCouldNotFindLabel(NotInLoopOrCouldNotFindLabel),
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
		InvalidUnaryOperation(crate::features::operations::PureUnary, TypeStringRepresentation),
		ReturnedTypeDoesNotMatch {
			expected_return_type: TypeStringRepresentation,
			returned_type: TypeStringRepresentation,
			/// Can be `None` if it is inferred parameters
			annotation_position: Option<SpanWithSource>,
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
			file: CouldNotOpenFile,
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
		TypeAlreadyDeclared {
			name: String,
			position: SpanWithSource,
		},
		TDZ(TDZ),
		InvalidMathematicalOrBitwiseOperation {
			operator: MathematicalAndBitwise,
			lhs: TypeStringRepresentation,
			rhs: TypeStringRepresentation,
			position: SpanWithSource,
		},
		InvalidCast {
			position: SpanWithSource,
			from: TypeStringRepresentation,
			to: TypeStringRepresentation,
		},
		/// TODO Position = Function body position. Could it be better
		/// TODO maybe warning?
		UnreachableVariableClosedOver(String, SpanWithSource),
		IncompatibleOverloadParameter {
			parameter_position: SpanWithSource,
			overloaded_parameter_position: SpanWithSource,
			parameter: TypeStringRepresentation,
			overloaded_parameter: TypeStringRepresentation,
		},
		IncompatibleOverloadReturnType {
			base_position: SpanWithSource,
			overload_position: SpanWithSource,
			base: TypeStringRepresentation,
			overload: TypeStringRepresentation,
		},
	}

	impl From<TypeCheckError<'_>> for Diagnostic {
		fn from(error: TypeCheckError<'_>) -> Self {
			let kind = super::DiagnosticKind::Error;
			match error {
				TypeCheckError::CouldNotFindVariable { variable, possibles: _, position } => {
					Diagnostic::Position {
						reason: format!(
							"Could not find variable '{variable}' in scope",
							// possibles Consider '{:?}'
						),
						position,
						kind,
					}
				}
				TypeCheckError::CouldNotFindType(reference, pos) => Diagnostic::Position {
					reason: format!("Could not find type '{reference}'"),
					position: pos,
					kind,
				},
				TypeCheckError::PropertyDoesNotExist { property, on, site } => {
					Diagnostic::Position {
						reason: match property {
							PropertyRepresentation::Type(ty) => format!("No property of type {ty} on {on}"),
							PropertyRepresentation::StringKey(property) => format!("No property '{property}' on {on}"),
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
							Diagnostic::PositionWithAdditionalLabels {
								reason: format!(
									"Argument of type {argument_type} is not assignable to parameter of type {restriction}" 
								),
								position: argument_position,
								labels: vec![(
									format!(
										"{parameter_type} was specialised with type {restriction}"
									),
									Some(restriction_pos),
								)],
								kind,
							}
						} else {
							Diagnostic::PositionWithAdditionalLabels {
								reason: format!(
									"Argument of type {argument_type} is not assignable to parameter of type {parameter_type}",
								),
								position: argument_position,
								labels: vec![(
									format!("Parameter has type {parameter_type}"),
									Some(parameter_position),
								)],
								kind,
							}
						}
					}
					FunctionCallingError::MissingArgument { parameter_position, call_site } => {
						Diagnostic::PositionWithAdditionalLabels {
							reason: "Missing argument".into(),
							position: call_site,
							kind,
							labels: vec![(
								"(non-optional) Parameter declared here".into(),
								Some(parameter_position),
							)],
						}
					}
					FunctionCallingError::ExcessArguments { count: _, position } => {
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
						reference: _,
						requirement: _,
						found: _,
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
					FunctionCallingError::TDZ { error: TDZ { position, variable_name }, call_site } => Diagnostic::PositionWithAdditionalLabels {
						reason: format!("Variable '{variable_name}' used before declaration"),
						position: call_site.unwrap(),
						kind,
						labels: vec![(
							"Variable referenced here".to_owned(),
							Some(position),
						)],
					},
					FunctionCallingError::SetPropertyConstraint { property_type, value_type, assignment_position, call_site } => Diagnostic::PositionWithAdditionalLabels {
						reason: "Invalid assignment to parameter".to_owned(),
						position: call_site.unwrap(),
						kind,
						labels: vec![(
							format!(
								"Type {value_type} does not meet property constraint {property_type}"
							),
							Some(assignment_position),
						)],
					},
					FunctionCallingError::UnconditionalThrow { value, call_site } => {
						Diagnostic::Position {
							reason: format!(
								"{value} unconditionally thrown in function"
							),
							position: call_site.unwrap(),
							kind,
						}
					}
				},
				TypeCheckError::AssignmentError(error) => match error {
					AssignmentError::DoesNotMeetConstraint {
						variable_type,
						variable_site,
						value_type,
						value_site,
					} => Diagnostic::PositionWithAdditionalLabels {
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
						property_constraint: property_type,
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
							reason: format!("Cannot assign to unknown variable '{variable}'"),
							position: assignment_position,
							kind,
						}
					}
				},
				TypeCheckError::InvalidJSXAttribute {
					attribute_name,
					attribute_type,
					value_type,
					attribute_type_site: _variable_site,
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
				} => Diagnostic::PositionWithAdditionalLabels {
					reason: format!(
						"Cannot return {returned_type} because the function is expected to return {expected_return_type}",
					),
					labels: annotation_position.into_iter().map(|annotation_position| (
						format!("Function annotated to return {expected_return_type} here"),
						Some(annotation_position),
					)).collect(),
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
					interpolation_site: _,
					expected: _,
					found: _,
				} => todo!(),
				TypeCheckError::RestParameterAnnotationShouldBeArrayType(pos) => {
					Diagnostic::Position {
						reason: "Rest parameter annotation should be array type".to_owned(),
						position: pos,
						kind,
					}
				}
				TypeCheckError::Unsupported { thing, at } => Diagnostic::Position {
					reason: format!("Unsupported: {thing}"),
					position: at,
					kind,
				},
				TypeCheckError::ReDeclaredVariable { name, position } => {
					Diagnostic::Position {
						reason: format!("Cannot declare variable {name}"),
						position,
						kind,
					}
				}
				TypeCheckError::FunctionDoesNotMeetConstraint {
					function_constraint,
					function_type,
					position,
				} => Diagnostic::Position {
					reason: format!(
						"{function_constraint} constraint on function does not match synthesised form {function_type}",
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
						reason: format!("Cannot redeclare variable '{name}'"),
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
					reason: "Property not writeable".into(),
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
					reason: format!("'{variable}' is only available on the {expected_context}, currently in {current_context}"),
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
				TypeCheckError::TypeAlreadyDeclared { name, position } => Diagnostic::Position {
					reason: format!("Type named '{name}' already declared"),
					position,
					kind,
				},
				TypeCheckError::TDZ(TDZ { position, variable_name }) => Diagnostic::Position {
					reason: format!("Variable '{variable_name}' used before declaration"),
					position,
					kind,
				},
				TypeCheckError::InvalidMathematicalOrBitwiseOperation { operator, lhs, rhs, position } => Diagnostic::Position {
					// TODO temp
					reason: format!("Cannot {lhs} {operator:?} {rhs}"),
					position,
					kind,
				},
				TypeCheckError::NotInLoopOrCouldNotFindLabel(_) => todo!(),
				TypeCheckError::InvalidCast { position, from, to } => {
					Diagnostic::Position {
						reason: format!("Cannot cast {from} to {to}"),
						position,
						kind,
					}
				}
				TypeCheckError::UnreachableVariableClosedOver(name, function_position) => {
					Diagnostic::Position {
						reason: format!("Function contains unreachable closed over variable '{}'", name),
						position: function_position,
						kind,
					}
				}
				TypeCheckError::IncompatibleOverloadParameter { parameter_position, overloaded_parameter_position, parameter, overloaded_parameter } => Diagnostic::PositionWithAdditionalLabels {
					reason: format!(
						"Overload with parameter of {overloaded_parameter} does not meet base parameter {parameter}",
					),
					labels: vec![(
						format!("Function has base type {parameter} here"),
						Some(parameter_position),
					)],
					position: overloaded_parameter_position,
					kind,
				},
				TypeCheckError::IncompatibleOverloadReturnType { base_position, overload_position, base, overload } => Diagnostic::PositionWithAdditionalLabels {
				reason: format!(
					"Cannot return {overload} in overload because base function is expected to return {base}",
				),
				labels: vec![(
					format!("Function annotated to return {base} here"),
					Some(base_position),
				)],
				position: overload_position,
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
		MergingInterfaceInSameContext {
			position: SpanWithSource,
		},
		TypesDoNotIntersect {
			left: TypeStringRepresentation,
			right: TypeStringRepresentation,
			position: SpanWithSource,
		},
		InvalidOrUnimplementedDefinitionFileItem(SpanWithSource),
		Unreachable(SpanWithSource),
	}

	impl From<TypeCheckWarning> for Diagnostic {
		fn from(warning: TypeCheckWarning) -> Self {
			let kind = super::DiagnosticKind::Warning;

			match warning {
				TypeCheckWarning::AwaitUsedOnNonPromise(position) => Diagnostic::Position {
					reason: "Unnecessary await expression / type is not promise".to_owned(),
					position,
					kind,
				},
				TypeCheckWarning::DeadBranch { expression_span, expression_value } => {
					Diagnostic::Position {
						reason: format!("Expression is always {expression_value:?}"),
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
					reason: format!("Unsupported: {thing}"),
					position: at,
					kind,
				},
				TypeCheckWarning::UselessExpression { expression_span } => Diagnostic::Position {
					reason: "Expression is always true".to_owned(),
					position: expression_span,
					kind,
				},
				TypeCheckWarning::MergingInterfaceInSameContext { position } => {
					Diagnostic::Position {
						reason: "Merging interfaces in the same context".to_owned(),
						position,
						kind,
					}
				}
				TypeCheckWarning::TypesDoNotIntersect { left, right, position } => {
					Diagnostic::Position {
						reason: format!("No intersection between types {left} and {right}"),
						position,
						kind,
					}
				}
				TypeCheckWarning::InvalidOrUnimplementedDefinitionFileItem(position) => {
					Diagnostic::Position {
						reason: "Invalid (or unimplemented) item in definition file skipped"
							.to_owned(),
						position,
						kind,
					}
				}
				TypeCheckWarning::Unreachable(position) => Diagnostic::Position {
					reason: "Unreachable statement".to_owned(),
					position,
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

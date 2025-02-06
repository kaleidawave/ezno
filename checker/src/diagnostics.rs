//! Contains type checking errors, warnings and related structures
use crate::{
	context::{environment::Label, AssignmentError, InformationChain},
	features::{modules::CouldNotOpenFile, CannotDeleteFromError},
	types::{
		calling::FunctionCallingError,
		printing::{print_type, print_type_into_buf, PrintingTypeInformation},
		properties::{assignment::SetPropertyError, PropertyKey},
		TypeId, TypeStore,
	},
	utilities::get_possibles_message,
};
use source_map::{SourceId, SpanWithSource};
use std::{fmt::Debug, iter, path::PathBuf};

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
		labels: Vec<(String, SpanWithSource)>,
		kind: DiagnosticKind,
	},
}

/// Temporary dead zone. Between the variable identifier being hoisted and the value being assigned
#[allow(clippy::upper_case_acronyms)]
pub struct VariableUsedInTDZ {
	pub variable_name: String,
	pub position: SpanWithSource,
}

pub struct InvalidRegExp {
	pub error: String,
	pub position: SpanWithSource,
}

pub struct NotInLoopOrCouldNotFindLabel {
	pub label: Label,
	pub position: SpanWithSource,
}

pub struct CannotOpenFile<'a> {
	pub file: CouldNotOpenFile,
	pub import_position: Option<SpanWithSource>,
	pub possibles: Vec<&'a str>,
	pub partial_import_path: &'a str,
}

impl<'a> From<CannotOpenFile<'a>> for Diagnostic {
	fn from(err: CannotOpenFile<'a>) -> Self {
		let CannotOpenFile { file, import_position, possibles, partial_import_path } = err;
		if let Some(import_position) = import_position {
			Diagnostic::PositionWithAdditionalLabels {
				reason: format!("Cannot find {partial_import_path}"),
				position: import_position,
				kind: DiagnosticKind::Error,
				labels: map_error_empty(possibles, |possibles| {
					vec![(get_possibles_message(&possibles), import_position)]
				}),
			}
		} else {
			Diagnostic::Global {
				reason: format!("Cannot find file {}", file.0.display()),
				kind: DiagnosticKind::Error,
			}
		}
	}
}

impl Diagnostic {
	pub fn sources(&self) -> impl Iterator<Item = SourceId> + '_ {
		use either::{Left, Right};
		match self {
			Diagnostic::Global { .. } => Left(Left(iter::empty())),
			Diagnostic::Position { position: span, .. } => Left(Right(iter::once(span.source))),
			Diagnostic::PositionWithAdditionalLabels { position: pos, labels, .. } => {
				Right(iter::once(pos.source).chain(labels.iter().map(|(_, span)| span.source)))
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

pub fn type_diagnostic<T: crate::context::InformationChain>(
	buf: &mut String,
	id: TypeId,
	info: PrintingTypeInformation<T>,
	// debug: bool,
) {
	let debug = false;
	print_type_into_buf(id, buf, &mut std::collections::HashSet::new(), None, info, debug)
}

pub enum PropertyKeyRepresentation {
	Type(String),
	StringKey(String),
}

impl PropertyKeyRepresentation {
	pub fn new<T: crate::context::InformationChain>(
		under: &PropertyKey,
		info: PrintingTypeInformation<T>,
	) -> PropertyKeyRepresentation {
		match under.clone() {
			PropertyKey::String(s) => PropertyKeyRepresentation::StringKey(s.to_string()),
			PropertyKey::Type(t) => PropertyKeyRepresentation::Type(print_type(t, info, false)),
		}
	}
}

// impl TypeId {
// 	/// TODO working it out
// 	pub(crate) fn from_property_constraint(
// 		property_constraint: crate::types::logical::Logical<crate::PropertyValue>,
// 		generics: GenericChain,
// 		ctx: &impl InformationChain,
// 		types: &TypeStore,
// 		debug_mode: bool,
// 	) -> TypeId {
// 		match property_constraint {
// 			crate::types::logical::Logical::Pure(constraint) => match constraint.inner_simple() {
// 				crate::PropertyValue::Value(v) => {
// 					let value =
// 						print_type_with_type_arguments(*v, generics, types, ctx, debug_mode);
// 					Self(value)
// 				}
// 				crate::PropertyValue::GetterAndSetter { .. }
// 				| crate::PropertyValue::Getter(_)
// 				| crate::PropertyValue::Setter(_) => Self("getter/setter".to_owned()),
// 				crate::PropertyValue::Deleted => Self("never".to_owned()),
// 				crate::PropertyValue::ConditionallyExists { .. }
// 				| crate::PropertyValue::Configured { .. } => unreachable!(),
// 			},
// 			crate::types::logical::Logical::Or { condition, left, right } => {
// 				let left_right = (*left, *right);
// 				// if let (Ok(left), Ok(right)) = left_right {
// 				// 	let mut left =
// 				// 		Self::from_property_constraint(left, None, ctx, types, debug_mode);
// 				// 	let right = Self::from_property_constraint(right, None, ctx, types, debug_mode);

// 				// 	crate::utilities::notify!("Here?");
// 				// 	left.0.push_str(" | ");
// 				// 	left.0.push_str(&right.0);
// 				// 	Self(left.0)
// 				// } else {
// 				crate::utilities::notify!("Printing {:?} base on {:?}", left_right, condition);
// 				Self("TODO or".to_owned())
// 				// }
// 			}
// 			crate::types::logical::Logical::Implies { on, antecedent } => {
// 				if generics.is_some() {
// 					crate::utilities::notify!("TODO chaining");
// 				}
// 				let generics = Some(GenericChainLink::PartiallyAppliedGenericArgumentsLink {
// 					parent_link: None,
// 					value: &antecedent,
// 					from: TypeId::UNIMPLEMENTED_ERROR_TYPE,
// 				});
// 				Self::from_property_constraint(*on, generics, ctx, types, debug_mode)
// 			}
// 			crate::types::logical::Logical::BasedOnKey(
// 				crate::types::logical::BasedOnKey::Left { value, key_arguments },
// 			) => {
// 				let property_generics = GenericChainLink::MappedPropertyLink {
// 					parent_link: generics.as_ref(),
// 					value: &key_arguments,
// 				};
// 				// let value = *value;
// 				// if let crate::types::logical::Logical::Pure(ref value) = value {

// 				// 	if let crate::types::properties::PropertyValue::Value(value) = value.inner_simple() {
// 				// 		let value = if let Some(crate::types::CovariantContribution::TypeId(value)) =
// 				// 			property_generics.get_argument_covariant(*value)
// 				// 		{
// 				// 			value
// 				// 		} else {
// 				// 			*value
// 				// 		};

// 				// 		let ty = types.get_type_by_id(value);
// 				// 		crate::utilities::notify!("{:?}", ty);

// 				// 		// Skip interface stuff
// 				// 		if let crate::Type::Constructor(crate::types::Constructor::Property { result, .. }) = ty {
// 				// 			let value = print_type_with_type_arguments(*result, Some(property_generics), types, ctx, debug_mode);
// 				// 			return Self(value)
// 				// 		}
// 				// 	}
// 				// }
// 				// crate::utilities::notify!("{:?}", value);
// 				Self::from_property_constraint(
// 					*value,
// 					Some(property_generics),
// 					ctx,
// 					types,
// 					debug_mode,
// 				)
// 			}
// 			crate::types::logical::Logical::BasedOnKey(
// 				crate::types::logical::BasedOnKey::Right(_right),
// 			) => Self("TODO BasedOnKey::Right".to_owned()),
// 		}
// 	}
// }

pub(crate) struct NoEnvironmentSpecified;

impl From<NoEnvironmentSpecified> for Diagnostic {
	fn from(_error: NoEnvironmentSpecified) -> Self {
		Diagnostic::Global { reason: "No environment".to_owned(), kind: DiagnosticKind::Error }
	}
}

/// Reasons for errors, intermediate type for generating [Diagnostic]s
/// e.g. cannot Call, cannot equate, duplicate key etc
pub(crate) enum TypeCheckError<'a> {
	FunctionCallingError(FunctionCallingError),
	JSXCallingError(FunctionCallingError),
	TemplateLiteralCallingError(FunctionCallingError),
	GetterCallingError(FunctionCallingError),
	SetterCallingError(FunctionCallingError),
	/// From calling super
	SuperCallError(FunctionCallingError),
	/// When accessing
	PropertyDoesNotExist {
		on: TypeId,
		property: PropertyKeyRepresentation,
		position: SpanWithSource,
		possibles: Vec<&'a str>,
	},
	/// When accessing
	CyclicTypeAlias {
		position: SpanWithSource,
	},
	#[allow(dead_code)]
	NotInLoopOrCouldNotFindLabel(NotInLoopOrCouldNotFindLabel),
	#[allow(dead_code)]
	RestParameterAnnotationShouldBeArrayType(SpanWithSource),
	CouldNotFindVariable {
		variable: &'a str,
		possibles: Vec<&'a str>,
		position: SpanWithSource,
	},
	CouldNotFindType(&'a str, Vec<&'a str>, SpanWithSource),
	/// For all `=`, including from declarations
	AssignmentError(AssignmentError),
	SetPropertyError(SetPropertyError),
	ReturnedTypeDoesNotMatch {
		expected_return_type: TypeId,
		returned_type: TypeId,
		/// Can be `None` if it is inferred parameters
		annotation_position: SpanWithSource,
		returned_position: SpanWithSource,
	},
	/// This could be a syntax error but that is difficult to type...
	NonTopLevelExport(SpanWithSource),
	FieldNotExported {
		file: &'a str,
		importing: &'a str,
		position: SpanWithSource,
		possibles: Vec<&'a str>,
	},
	/// For the `satisfies` keyword
	NotSatisfied {
		position: SpanWithSource,
		expected: TypeId,
		found: TypeId,
	},
	/// Catch type is not compatible with thrown type
	CatchTypeDoesNotMatch {
		position: SpanWithSource,
		expected: TypeId,
		found: TypeId,
	},
	/// Something the checker does not supported
	Unsupported {
		thing: &'static str,
		position: SpanWithSource,
	},
	InvalidDefaultParameter {
		position: SpanWithSource,
		expected: TypeId,
		found: TypeId,
	},
	/// TODO temp, needs more info
	#[allow(dead_code)]
	FunctionDoesNotMeetConstraint {
		function_constraint: TypeId,
		function_type: TypeId,
		position: SpanWithSource,
	},
	CannotRedeclareVariable {
		name: String,
		position: SpanWithSource,
	},
	/// This is for structure generics (type annotations)
	GenericArgumentDoesNotMeetRestriction {
		parameter_restriction: TypeId,
		argument: TypeId,
		position: SpanWithSource,
	},
	/// This is for structure generics (type annotations)
	GenericArgumentCountMismatch {
		expected_count: usize,
		count: usize,
		position: SpanWithSource,
	},
	#[allow(dead_code)]
	NotTopLevelImport(SpanWithSource),
	DuplicateImportName {
		import_position: SpanWithSource,
		existing_position: SpanWithSource,
	},
	#[allow(dead_code)]
	DoubleDefaultExport(SpanWithSource),
	NoDefaultExport {
		position: SpanWithSource,
		partial_import_path: &'a str,
	},
	/// WIP
	#[allow(dead_code)]
	VariableNotDefinedInContext {
		variable: &'a str,
		expected_context: &'a str,
		current_context: String,
		position: SpanWithSource,
	},
	TypeNeedsTypeArguments(&'a str, SpanWithSource),
	TypeAlreadyDeclared {
		name: String,
		position: SpanWithSource,
	},
	#[allow(clippy::upper_case_acronyms)]
	VariableUsedInTDZ(VariableUsedInTDZ),
	InvalidMathematicalOrBitwiseOperation {
		operator: crate::features::operations::MathematicalOrBitwiseOperation,
		lhs: TypeId,
		rhs: TypeId,
		position: SpanWithSource,
	},
	// Only for `<` `>` etc
	InvalidEqualityOperation {
		operator: crate::features::operations::EqualityAndInequality,
		lhs: TypeId,
		rhs: TypeId,
		position: SpanWithSource,
	},
	InvalidUnaryOperation {
		operator: crate::features::operations::UnaryOperation,
		operand: TypeId,
		position: SpanWithSource,
	},
	#[allow(dead_code)]
	InvalidCast {
		position: SpanWithSource,
		from: TypeId,
		to: TypeId,
	},
	/// TODO Position = Function body position. Could it be better
	/// TODO maybe warning?
	#[allow(dead_code)]
	UnreachableVariableClosedOver(String, SpanWithSource),
	IncompatibleOverloadParameter {
		parameter_position: SpanWithSource,
		overloaded_parameter_position: SpanWithSource,
		parameter: TypeId,
		overloaded_parameter: TypeId,
	},
	IncompatibleOverloadReturnType {
		base_position: SpanWithSource,
		overload_position: SpanWithSource,
		base: TypeId,
		overload: TypeId,
	},
	FunctionWithoutBodyNotAllowedHere {
		position: SpanWithSource,
	},
	CannotDeleteProperty(CannotDeleteFromError),
	InvalidRegExp(InvalidRegExp),
}

fn map_error_empty<U, T: Default>(n: Vec<U>, cb: impl FnOnce(Vec<U>) -> T) -> T {
	if n.is_empty() {
		<T as Default>::default()
	} else {
		cb(n)
	}
}

pub struct UnimplementedWarning {
	pub item: &'static str,
	pub position: SpanWithSource,
}

impl From<UnimplementedWarning> for Diagnostic {
	fn from(UnimplementedWarning { position, item }: UnimplementedWarning) -> Self {
		Diagnostic::Position {
			reason: format!("Unsupported: {item}"),
			position,
			kind: DiagnosticKind::Warning,
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
	ExcessProperty {
		position: SpanWithSource,
		expected_type: TypeId,
		excess_property_name: String,
	},
	IgnoringAsExpression(SpanWithSource),
	UselessExpression {
		expression_span: SpanWithSource,
		// TODO other branch information
	},
	MergingInterfaceInSameContext {
		position: SpanWithSource,
	},
	TypesDoNotIntersect {
		left: TypeId,
		right: TypeId,
		position: SpanWithSource,
	},
	InvalidOrUnimplementedDefinitionFileItem(SpanWithSource),
	/// TODO WIP
	ConditionalExceptionInvoked {
		value: TypeId,
		/// Should be set
		call_site: SpanWithSource,
	},
	Unreachable(SpanWithSource),
	DisjointEquality {
		lhs: TypeId,
		rhs: TypeId,
		result: bool,
		position: SpanWithSource,
	},
	ItemMustBeUsedWithFlag {
		item: &'static str,
		position: SpanWithSource,
	},
}

impl<'a> TypeCheckError<'a> {
	pub fn into_diagnostic<T: crate::context::InformationChain>(
		self,
		information: PrintingTypeInformation<T>,
	) -> Diagnostic {
		let kind = DiagnosticKind::Error;
		match self {
			TypeCheckError::CouldNotFindVariable { variable, possibles, position } => {
				let labels = map_error_empty(possibles, |possibles| vec![(
					get_possibles_message(&possibles),
					position,
				)]);
				Diagnostic::PositionWithAdditionalLabels {
					reason: format!("Could not find variable '{variable}' in scope"),
					labels,
					position,
					kind,
				}
			}
			TypeCheckError::CouldNotFindType(reference, possibles, position) => {
				let labels = map_error_empty(possibles, |possibles| vec![(
					get_possibles_message(&possibles),
					position,
				)]);
				Diagnostic::PositionWithAdditionalLabels {
				reason: format!("Could not find type '{reference}'"),
				position,
				labels,
				kind,
			}},
			TypeCheckError::PropertyDoesNotExist { property, on, position, possibles } => {
				let labels = map_error_empty(possibles, |possibles| vec![(
					get_possibles_message(&possibles),
					position,
				)]);
				let mut reason: String = match property {
					PropertyKeyRepresentation::Type(ty) => {
						format!("No property of type '{ty}'")
						// let mut source: String = "No property of type ";
						// type_diagnostic(&mut source, ty, information);
						// source
					}
					PropertyKeyRepresentation::StringKey(property) => format!("No property '{property}'"),
				};
				{
					reason.push_str(" on ");
					type_diagnostic(&mut reason, on, information);
				}
				Diagnostic::PositionWithAdditionalLabels {
					reason,
					position,
					labels,
					kind,
				}
			}
			TypeCheckError::FunctionCallingError(error) => function_calling_error_diagnostic(error, kind, information, ""),
			TypeCheckError::JSXCallingError(error) => function_calling_error_diagnostic(error, kind, information, " (in JSX)"),
			TypeCheckError::GetterCallingError(error) => function_calling_error_diagnostic(error, kind, information, " (in getter)"),
			TypeCheckError::SetterCallingError(error) => function_calling_error_diagnostic(error, kind, information, " (in setter)"),
			TypeCheckError::TemplateLiteralCallingError(error) => {
				function_calling_error_diagnostic(error, kind, information, " (in template literal)")
			},
			TypeCheckError::SuperCallError(error) => function_calling_error_diagnostic(error, kind, information, " (in super call)"),
			TypeCheckError::AssignmentError(error) => match error {
				AssignmentError::DoesNotMeetConstraint {
					variable_type,
					variable_position,
					value_type,
					value_position,
				} => Diagnostic::PositionWithAdditionalLabels {
					reason: {
						let mut reason = "Type ".to_owned();
						type_diagnostic(&mut reason, value_type, information);
						reason.push_str(" is not assignable to type ");
						type_diagnostic(&mut reason, variable_type, information);
						reason
					},
					position: value_position,
					labels: vec![(
						{
							let mut reason = "Variable declared with type ".to_owned();
							type_diagnostic(&mut reason, variable_type, information);
							reason
						},
						variable_position,
					)],
					kind,
				},
				// AssignmentError::PropertyConstraint {
				// 	property_constraint: property_type,
				// 	value_type,
				// 	assignment_position,
				// } => Diagnostic::Position {
				// 	reason: format!(
				// 		"Type {value_type} does not meet property constraint {property_type}"
				// 	),
				// 	position: assignment_position,
				// 	kind,
				// },
				AssignmentError::Constant(position) => Diagnostic::Position {
					reason: "Cannot assign to constant".to_owned(),
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
				AssignmentError::VariableUsedInTDZ(VariableUsedInTDZ { variable_name, position }) => {
					Diagnostic::Position {
						reason: format!("Cannot assign to '{variable_name}' before declaration"),
						position,
						kind,
					}
				}
			},
			TypeCheckError::ReturnedTypeDoesNotMatch {
				annotation_position,
				returned_position,
				expected_return_type,
				returned_type,
			} => Diagnostic::PositionWithAdditionalLabels {
				reason: {
					let mut source = "Cannot return ".to_owned();
					type_diagnostic(&mut source, returned_type, information);
					source.push_str(" because the function is expected to return ");
					type_diagnostic(&mut source, expected_return_type, information);
					source
				},
				labels: vec![(
					{
						let mut source = "Function annotated to return ".to_owned();
						type_diagnostic(&mut source, returned_type, information);
						source.push_str(" here");
						source
					},
					annotation_position,
				)],
				position: returned_position,
				kind,
			},
			TypeCheckError::InvalidDefaultParameter {
				expected,
				found,
				position,
			} => Diagnostic::Position {
				reason: {
					let mut source = "Cannot use a default value of type ".to_owned();
					type_diagnostic(&mut source, found, information);
					source.push_str(" for parameter of type ");
					type_diagnostic(&mut source, expected, information);
					source
				},
				position,
				kind,
			},
			TypeCheckError::CatchTypeDoesNotMatch {
				expected,
				found,
				position,
			} => Diagnostic::Position {
				reason: {
					let mut source = "Cannot catch type ".to_owned();
					type_diagnostic(&mut source, expected, information);
					source.push_str(" because the try block throws ");
					type_diagnostic(&mut source, found, information);
					source
				},
				position,
				kind,
			},
			TypeCheckError::NonTopLevelExport(position) => Diagnostic::Position {
				reason: "Cannot export at not top level".to_owned(),
				position,
				kind,
			},
			TypeCheckError::FieldNotExported { file, importing, position, possibles } => {
				Diagnostic::PositionWithAdditionalLabels {
					reason: format!("{importing} not exported from {file}"),
					position,
					kind,
					labels: map_error_empty(possibles, |possibles| vec![(
						get_possibles_message(&possibles),
						position,
					)]),
				}
			}
			TypeCheckError::RestParameterAnnotationShouldBeArrayType(position) => {
				Diagnostic::Position {
					reason: "Rest parameter annotation should be array type".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckError::Unsupported { thing, position } => Diagnostic::Position {
				reason: format!("Unsupported: {thing}"),
				position,
				kind,
			},
			TypeCheckError::FunctionDoesNotMeetConstraint {
				function_constraint,
				function_type,
				position,
			} => Diagnostic::Position {
				reason: {
					let mut source = String::new();
					type_diagnostic(&mut source, function_constraint, information);
					source.push_str(" constraint on function does not match synthesised from ");
					type_diagnostic(&mut source, function_type, information);
					source
				},
				position,
				kind,
			},
			TypeCheckError::NotSatisfied { position, expected, found } => Diagnostic::Position {
				reason: {
					let mut source = "Expected ".to_owned();
					type_diagnostic(&mut source, expected, information);
					source.push_str(", found ");
					type_diagnostic(&mut source, found, information);
					source
				},
				position,
				kind,
			},
			TypeCheckError::CannotRedeclareVariable { name, position } => {
				Diagnostic::Position {
					reason: format!("Cannot redeclare variable '{name}'"),
					position,
					kind,
				}
			}
			TypeCheckError::GenericArgumentDoesNotMeetRestriction {
				argument,
				parameter_restriction,
				position,
			} => Diagnostic::Position {
				reason: {
					let mut source = "Generic argument ".to_owned();
					type_diagnostic(&mut source, argument, information);
					source.push_str(" does not match ");
					type_diagnostic(&mut source, parameter_restriction, information);
					source
				},
				position,
				kind,
			},
			TypeCheckError::GenericArgumentCountMismatch {
				count,
				expected_count,
				position,
			} => {
				let reason = if expected_count == 0 {
					"Cannot pass a type argument to a non-generic type".to_owned()
				} else if expected_count == 1 {
					format!("Expected 1 type argument, but got {count}")
				} else {
					format!("Expected {expected_count} type arguments, but got {count}")
				};
				Diagnostic::Position {
					position,
					kind,
					reason
				}
			},
			TypeCheckError::NotTopLevelImport(position) => Diagnostic::Position {
				reason: "Import must be in the top of the scope".to_owned(),
				position,
				kind,
			},
			TypeCheckError::DoubleDefaultExport(position) => Diagnostic::Position {
				reason: "Cannot have more than one default export".to_owned(),
				position,
				kind,
			},
			TypeCheckError::DuplicateImportName { import_position: position, existing_position, ..} => Diagnostic::PositionWithAdditionalLabels {
				reason: "Cannot import using conflicting name".to_string(),
				position,
				kind,
				labels: vec![("Existing import with same name".to_string(), existing_position)],
			},
			TypeCheckError::NoDefaultExport { partial_import_path, position, ..} => Diagnostic::Position {
				reason: format!("Cannot find default export from module '{partial_import_path}'"),
				position,
				kind
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
			TypeCheckError::TypeNeedsTypeArguments(name, position) => Diagnostic::Position {
				reason: format!("Type {name} requires type arguments"),
				// reason: {
				// 	let mut reason = " ".to_owned();
				// 	reason.push_str(" ");
				// 	reason
				// },
				position,
				kind,
			},
			TypeCheckError::TypeAlreadyDeclared { name, position } => Diagnostic::Position {
				reason: format!("Type named '{name}' already declared"),
				position,
				kind,
			},
			TypeCheckError::VariableUsedInTDZ(VariableUsedInTDZ { position, variable_name }) => Diagnostic::Position {
				reason: format!("Variable '{variable_name}' used before declaration"),
				position,
				kind,
			},
			TypeCheckError::InvalidMathematicalOrBitwiseOperation { operator, lhs, rhs, position } => Diagnostic::Position {
				reason:  {
					let mut reason = "Cannot ".to_owned();
					type_diagnostic(&mut reason, lhs, information);
					// TODO temp
					// write!(&mut reason, " {operator:?} with ");
					type_diagnostic(&mut reason, rhs, information);
					reason
				},
				position,
				kind,
			},
			TypeCheckError::InvalidUnaryOperation {
				operator,
				operand,
				position,
			} => {
				Diagnostic::Position {
					// TODO temp
					reason:  {
						let mut reason = "Cannot ".to_owned();
						type_diagnostic(&mut reason, operand, information);
						// TODO temp
						// write!(&mut reason, " {operator:?}");
						reason
					},
					position,
					kind,
				}
			},
			TypeCheckError::InvalidEqualityOperation { operator, lhs, rhs, position } => Diagnostic::Position {
				reason:  {
					let mut reason = "Cannot ".to_owned();
					type_diagnostic(&mut reason, lhs, information);
					// TODO temp
					// write!(&mut reason, " {operator:?} with ");
					type_diagnostic(&mut reason, rhs, information);
					reason
				},
				position,
				kind,
			},
			TypeCheckError::NotInLoopOrCouldNotFindLabel(NotInLoopOrCouldNotFindLabel {
				label: _,
				position,
			}) => {
				Diagnostic::Position {
					// TODO temp
					reason: "Cannot use `break` or `continue` here or could not find label".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckError::InvalidCast { position, from, to } => {
				Diagnostic::Position {
					reason: {
						let mut reason = "Cannot cast ".to_owned();
						type_diagnostic(&mut reason, from, information);
						reason.push_str(" to ");
						type_diagnostic(&mut reason, to, information);
						reason
					},
					position,
					kind,
				}
			},
			TypeCheckError::UnreachableVariableClosedOver(name, function_position) => {
				Diagnostic::Position {
					reason: format!("Function contains unreachable closed over variable '{name}'"),
					position: function_position,
					kind,
				}
			},
			TypeCheckError::IncompatibleOverloadParameter { parameter_position, overloaded_parameter_position, parameter, overloaded_parameter } =>
			 Diagnostic::PositionWithAdditionalLabels {
				reason: format!(
					"Overload with parameter of {overloaded_parameter:?} does not meet base parameter {parameter:?}"
				),
				labels: vec![(
					format!("Function has base type {parameter:?} here"),
					parameter_position,
				)],
				position: overloaded_parameter_position,
				kind,
			},
			TypeCheckError::IncompatibleOverloadReturnType { base_position, overload_position, base, overload } => Diagnostic::PositionWithAdditionalLabels {
				reason: format!(
					"Cannot return {overload:?} in overload because base function is expected to return {base:?}"
				),
				labels: vec![(
					format!("Function annotated to return {base:?} here"),
					base_position,
				)],
				position: overload_position,
				kind,
			},
			TypeCheckError::FunctionWithoutBodyNotAllowedHere { position } => {
				Diagnostic::Position {
					reason: "Function without body not allowed here".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckError::CyclicTypeAlias { position } => {
				Diagnostic::Position {
					reason: "Circular type reference".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckError::CannotDeleteProperty(CannotDeleteFromError::Constraint { constraint, position }) => {
				Diagnostic::Position {
					reason: format!("Cannot delete from object constrained to {constraint:?}"),
					position,
					kind,
				}
			}
			TypeCheckError::CannotDeleteProperty(CannotDeleteFromError::NonConfigurable {
				position,
			}) => {
				Diagnostic::Position {
					reason: "Cannot delete from non-configurable property".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckError::SetPropertyError(error) => match error {
				SetPropertyError::NotWriteable {
					property,
					position,
				} => Diagnostic::Position {
					reason: match property {
						PropertyKeyRepresentation::Type(ty) => format!("Cannot write to property of type {ty}"),
						PropertyKeyRepresentation::StringKey(property) => format!("Cannot write to property '{property}'")
					},
					position,
					kind,
				},
				SetPropertyError::DoesNotMeetConstraint {
					property_constraint,
					value_type,
					reason: _,
					position,
				} => Diagnostic::Position {
					reason: {
						let mut reason = "Type ".to_owned();
						type_diagnostic(&mut reason, value_type, information);
						reason.push_str(" does not meet property constraint ");
						type_diagnostic(&mut reason, property_constraint, information);
						reason
					},
					position,
					kind,
				},
				SetPropertyError::AssigningToGetter {
					property,
					position,
				} => Diagnostic::Position {
					reason: match property {
						PropertyKeyRepresentation::Type(ty) => format!("Cannot write to property of type {ty} as it is a getter"),
						PropertyKeyRepresentation::StringKey(property) => format!("Cannot write to property '{property}' as it is a getter")
					},
					position,
					kind,
				},
				SetPropertyError::AssigningToNonExistent {
					property,
					position,
				} => Diagnostic::Position {
					reason: match property {
						PropertyKeyRepresentation::Type(ty) => format!("Cannot write to non-existent property of type {ty}"),
						PropertyKeyRepresentation::StringKey(property) => format!("Cannot write to non-existent property '{property}'")
					},
					position,
					kind,
				}
			},
			TypeCheckError::InvalidRegExp(InvalidRegExp { error, position }) => Diagnostic::Position {
				reason: format!("Invalid regular expression: {error}"),
				position,
				kind,
			},
		}
	}
}

impl TypeCheckWarning {
	pub fn into_diagnostic<T: crate::context::InformationChain>(
		self,
		information: PrintingTypeInformation<T>,
	) -> Diagnostic {
		let kind = super::DiagnosticKind::Warning;
		match self {
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
			TypeCheckWarning::ExcessProperty { position, expected_type, excess_property_name } => {
				let mut reason = format!("'{excess_property_name}' is not a property of ");
				type_diagnostic(&mut reason, expected_type, information);
				Diagnostic::Position { reason, position, kind }
			}
			TypeCheckWarning::IgnoringAsExpression(position) => Diagnostic::Position {
				reason: "'as' expressions are ignore by the checker".to_owned(),
				position,
				kind,
			},
			TypeCheckWarning::ItemMustBeUsedWithFlag { item, position } => Diagnostic::Position {
				reason: format!("{item} must be used with 'extras' option"),
				position,
				kind,
			},
			TypeCheckWarning::UselessExpression { expression_span } => Diagnostic::Position {
				reason: "Expression is always true".to_owned(),
				position: expression_span,
				kind,
			},
			TypeCheckWarning::MergingInterfaceInSameContext { position } => Diagnostic::Position {
				reason: "Merging interfaces in the same context".to_owned(),
				position,
				kind,
			},
			TypeCheckWarning::TypesDoNotIntersect { left, right, position } => {
				Diagnostic::Position {
					reason: {
						let mut reason = "No intersection between types ".to_owned();
						type_diagnostic(&mut reason, left, information);
						reason.push_str(" and ");
						type_diagnostic(&mut reason, right, information);
						reason
					},
					position,
					kind,
				}
			}
			TypeCheckWarning::InvalidOrUnimplementedDefinitionFileItem(position) => {
				Diagnostic::Position {
					reason: "Invalid (or unimplemented) item in definition file skipped".to_owned(),
					position,
					kind,
				}
			}
			TypeCheckWarning::Unreachable(position) => {
				Diagnostic::Position { reason: "Unreachable statement".to_owned(), position, kind }
			}
			TypeCheckWarning::ConditionalExceptionInvoked { value, call_site } => {
				let mut reason: String = "Conditional '".to_owned();
				type_diagnostic(&mut reason, value, information);
				reason.push_str("' was thrown in function");
				Diagnostic::Position { reason, position: call_site, kind }
			}
			TypeCheckWarning::DisjointEquality { lhs, rhs, position, result } => {
				Diagnostic::Position {
					reason: {
						let mut reason: String = format!("This equality is always {result} as ");
						type_diagnostic(&mut reason, lhs, information);
						reason.push_str(" and ");
						type_diagnostic(&mut reason, rhs, information);
						reason.push_str(" have no overlap");
						reason
					},
					position,
					kind,
				}
			}
		}
	}
}

/// Only for internal things
///
/// WIP
pub struct InfoDiagnostic(pub String, pub SpanWithSource);

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

/// `context` is the what kind of a function call the error happened in. (for example tagged template literals or JSX)
fn function_calling_error_diagnostic<C: crate::context::InformationChain>(
	error: FunctionCallingError,
	kind: crate::DiagnosticKind,
	information: PrintingTypeInformation<'_, C>,
	context: &str,
) -> Diagnostic {
	match error {
		FunctionCallingError::InvalidArgumentType {
			parameter_type,
			argument_type,
			argument_position,
			parameter_position,
			restriction,
		} => {
			if let Some((restriction_pos, restriction)) = restriction {
				Diagnostic::PositionWithAdditionalLabels {
					reason: {
						let mut source = "Argument of type ".to_owned();
						type_diagnostic(&mut source, argument_type, information);
						source.push_str(" is not assignable to parameter of type ");
						type_diagnostic(&mut source, restriction, information);
						source.push_str(context);
						source
					},
					position: argument_position,
					labels: vec![(
						{
							let mut label = String::new();
							type_diagnostic(&mut label, parameter_type, information);
							label.push_str(" was specialised with type ");
							type_diagnostic(&mut label, restriction, information);
							label
						},
						restriction_pos,
					)],
					kind,
				}
			} else {
				Diagnostic::PositionWithAdditionalLabels {
					reason: {
						let mut source = "Argument of type ".to_owned();
						type_diagnostic(&mut source, argument_type, information);
						source.push_str(" is not assignable to parameter of type ");
						type_diagnostic(&mut source, parameter_type, information);
						source.push_str(context);
						source
					},
					position: argument_position,
					labels: vec![(
						{
							let mut source = "Parameter has type ".to_owned();
							type_diagnostic(&mut source, parameter_type, information);
							source
						},
						parameter_position,
					)],
					kind,
				}
			}
		}
		FunctionCallingError::MissingArgument { parameter_position, call_site } => {
			Diagnostic::PositionWithAdditionalLabels {
				reason: format!("Missing argument{context}"),
				position: call_site,
				kind,
				labels: vec![("(non-optional) Parameter declared here".into(), parameter_position)],
			}
		}
		FunctionCallingError::ExcessArguments { count: _, position } => {
			Diagnostic::Position { reason: format!("Excess argument{context}"), position, kind }
		}
		FunctionCallingError::ExcessTypeArguments { expected_count, count, position } => {
			let reason = if expected_count == 0 {
				format!("Cannot pass a type argument to a non-generic function{context}")
			} else if expected_count == 1 {
				format!("Expected 1 type argument, but got {count}{context}")
			} else {
				format!("Expected {expected_count} type arguments, but got {count}{context}")
			};
			Diagnostic::Position { position, kind, reason }
		}
		FunctionCallingError::NotCallable { calling, call_site } => Diagnostic::Position {
			reason: {
				let mut source = "Cannot call type ".to_owned();
				type_diagnostic(&mut source, calling, information);
				source.push_str(context);
				source
			},
			position: call_site,
			kind,
		},
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
			reason: format!("Encountered recursion{context}"),
			position: call_site,
			kind,
		},
		FunctionCallingError::NoLogicForIdentifier(name, position) => Diagnostic::Position {
			reason: format!("No logic for constant function {name}{context}"),
			kind,
			position,
		},
		FunctionCallingError::NeedsToBeCalledWithNewKeyword(position) => Diagnostic::Position {
			reason: "Class constructor must be called with new".to_owned(),
			kind,
			position,
		},
		FunctionCallingError::VariableUsedInTDZ {
			error: VariableUsedInTDZ { position, variable_name },
			call_site,
		} => Diagnostic::PositionWithAdditionalLabels {
			reason: format!("Variable '{variable_name}' used before declaration{context}"),
			position: call_site,
			kind,
			labels: vec![("Variable referenced here".to_owned(), position)],
		},
		FunctionCallingError::SetPropertyConstraint {
			property_type,
			value_type,
			assignment_position,
			call_site,
		} => Diagnostic::PositionWithAdditionalLabels {
			reason: format!("Invalid assignment through parameter{context}"),
			position: call_site,
			kind,
			labels: vec![(
				{
					let mut source = "Type ".to_owned();
					type_diagnostic(&mut source, value_type, information);
					source.push_str(" does not meet property constraint ");
					type_diagnostic(&mut source, property_type, information);
					source
				},
				assignment_position,
			)],
		},
		FunctionCallingError::MismatchedThis { call_site, expected, found } => {
			Diagnostic::Position {
				reason: {
					let mut source =
						"The 'this' context of the function is expected to be ".to_owned();
					type_diagnostic(&mut source, expected, information);
					source.push_str(" found ");
					type_diagnostic(&mut source, found, information);
					source.push_str(context);
					source
				},
				position: call_site,
				kind,
			}
		}
		FunctionCallingError::CannotCatch { catch, thrown, thrown_position } => {
			Diagnostic::Position {
				reason: {
					let mut source = "Cannot throw ".to_owned();
					type_diagnostic(&mut source, thrown, information);
					source.push_str(" in block that expects ");
					type_diagnostic(&mut source, catch, information);
					source.push_str(context);
					source
				},
				position: thrown_position,
				kind,
			}
		}
		FunctionCallingError::DeleteConstraint { constraint, delete_position, call_site: _ } => {
			Diagnostic::Position {
				reason: {
					let mut source = "Cannot delete from object constrained to ".to_owned();
					type_diagnostic(&mut source, constraint, information);
					source.push_str(context);
					source
				},
				position: delete_position,
				kind,
			}
		}
		FunctionCallingError::NotConfigurable { property, call_site } => {
			Diagnostic::Position {
				reason: todo!(),
				// reason: match property {
				// 	PropertyKeyRepresentation::Type(ty) => format!("Property of type '{ty}' not configurable"),
				// 	PropertyKeyRepresentation::StringKey(property) => format!("Property '{property}' not configurable"),
				// },
				position: call_site,
				kind,
			}
		}
		FunctionCallingError::InvalidRegExp(InvalidRegExp { error, position }) => {
			Diagnostic::Position {
				reason: format!("Invalid regular expression: {error}"),
				position,
				kind,
			}
		}
	}
}

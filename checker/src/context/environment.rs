use source_map::{SourceId, Span, SpanWithSource};
use std::collections::HashSet;

use crate::{
	diagnostics::{
		NotInLoopOrCouldNotFindLabel, PropertyRepresentation, TypeCheckError, TypeCheckWarning,
		TypeStringRepresentation, TDZ,
	},
	events::{ApplicationResult, Event, FinalEvent, RootReference},
	features::{
		assignments::{
			Assignable, AssignableArrayDestructuringField, AssignableObjectDestructuringField,
			AssignmentKind, Reference,
		},
		modules::Exported,
		objects::SpecialObjects,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, MathematicalAndBitwise,
		},
		variables::{VariableMutability, VariableOrImport, VariableWithValue},
	},
	subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
	types::{
		is_type_truthy_falsy, printing,
		properties::{PropertyKey, PropertyKind, PropertyValue},
		PolyNature, Type, TypeCombinable, TypeStore,
	},
	CheckingData, Decidable, Instance, RootContext, TypeCheckOptions, TypeId,
};

use super::{
	get_on_ctx, get_value_of_variable,
	information::{merge_info, InformationChain, Publicity},
	invocation::CheckThings,
	AssignmentError, ClosedOverReferencesInScope, Context, ContextType, Environment,
	GeneralContext, SetPropertyError,
};

pub type ContextLocation = Option<String>;

#[derive(Debug)]
pub struct Syntax<'a> {
	pub scope: Scope,
	pub(super) parent: GeneralContext<'a>,

	/// Variables that this context pulls in from above (across a dynamic context). aka not from parameters of bound this
	/// Not to be confused with `closed_over_references`
	pub free_variables: HashSet<RootReference>,

	/// Variables used in this scope which are closed over by functions. These need to be stored
	/// Not to be confused with `used_parent_references`
	pub closed_over_references: ClosedOverReferencesInScope,

	/// TODO WIP! server, client, worker etc
	pub location: ContextLocation,

	/// Represents whether [`crate::events::FinalEvent`]s have occurred. Can be used to tell
	/// whether the statements will fire. Shortcut for inferred return types
	///
	/// In the future narrowing
	pub state: ApplicationResult,
}

/// Code under a dynamic boundary can run more than once
#[derive(Debug, Clone, Copy)]
pub enum DynamicBoundaryKind {
	Loop,
	Function,
}

impl DynamicBoundaryKind {
	#[must_use]
	pub fn can_use_variable_before_definition(self) -> bool {
		matches!(self, Self::Function)
	}
}

impl<'a> ContextType for Syntax<'a> {
	fn as_general_context(et: &Context<Self>) -> GeneralContext<'_> {
		GeneralContext::Syntax(et)
	}

	fn get_parent(&self) -> Option<&GeneralContext<'_>> {
		Some(&self.parent)
	}

	fn as_syntax(&self) -> Option<&Syntax> {
		Some(self)
	}

	fn get_state_mut(&mut self) -> Option<&mut ApplicationResult> {
		Some(&mut self.state)
	}

	fn get_closed_over_references_mut(&mut self) -> Option<&mut ClosedOverReferencesInScope> {
		Some(&mut self.closed_over_references)
	}
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedReturnType {
	/// This may have a position in the future
	Inferred(TypeId),
	FromReturnAnnotation(TypeId, Span),
}

impl ExpectedReturnType {
	pub(crate) fn get_type_and_position(self) -> (TypeId, Option<Span>) {
		match self {
			ExpectedReturnType::Inferred(ty) => (ty, None),
			ExpectedReturnType::FromReturnAnnotation(ty, pos) => (ty, Some(pos)),
		}
	}
}

/// TODO better names
/// Decides whether `await` and `yield` are available and many others
///
/// `this` is the dependent type
#[derive(Debug, Clone)]
pub enum FunctionScope {
	ArrowFunction {
		// This always points to a poly free variable type
		free_this_type: TypeId,
		is_async: bool,
		expected_return: Option<ExpectedReturnType>,
	},
	/// TODO does this need to gdistinguish class
	MethodFunction {
		// This always points to a poly free variable type
		free_this_type: TypeId,
		is_async: bool,
		is_generator: bool,

		expected_return: Option<ExpectedReturnType>,
	},
	// is new-able
	Function {
		is_generator: bool,
		is_async: bool,
		expected_return: Option<ExpectedReturnType>,
		// This always points to a conditional type based on `new.target === undefined`
		this_type: TypeId,
		type_of_super: TypeId,
		location: ContextLocation,
	},
	Constructor {
		/// Can call `super`
		extends: bool,
		type_of_super: Option<TypeId>,
		// This is always creates, but may not be used (or have the relevant properties & prototype)
		this_object_type: TypeId,
	},
}

impl FunctionScope {
	// TODO temp
	pub(crate) fn get_expected_return_type_mut(&mut self) -> &mut Option<ExpectedReturnType> {
		match self {
			FunctionScope::ArrowFunction { expected_return, .. }
			| FunctionScope::MethodFunction { expected_return, .. }
			| FunctionScope::Function { expected_return, .. } => expected_return,
			FunctionScope::Constructor { .. } => unreachable!(),
		}
	}
}

/// For labeled statements
pub type Label = Option<String>;

#[derive(Clone, Copy)]
pub enum Returnable<'a, A: crate::ASTImplementation> {
	Statement(Option<&'a A::MultipleExpression<'a>>, Span),
	ArrowFunctionBody(&'a A::Expression<'a>),
}

/// TODO name of structure
/// TODO conditionals should have conditional proofs (separate from the ones on context)
#[derive(Debug, Clone)]
pub enum Scope {
	Function(FunctionScope),
	InterfaceEnvironment {
		this_constraint: TypeId,
	},
	DefaultFunctionParameter {},
	FunctionAnnotation {},
	/// For ifs, elses, or lazy operators
	Conditional {
		/// Something that is truthy for this to run
		antecedent: TypeId,

		is_switch: Option<Label>,
	},
	/// Variables here are dependent on the iteration,
	Iteration {
		label: Label, // TODO on: Proofs,
	},
	TryBlock {},
	// Just blocks and modules
	Block {},
	Module {
		source: SourceId,
		exported: Exported,
	},
	DefinitionModule {
		source: SourceId,
	},
	/// For generic parameters
	TypeAlias,
	StaticBlock {},
	/// For repl only
	PassThrough {
		source: SourceId,
	},
}

impl Scope {
	pub fn is_dynamic_boundary(&self) -> Option<DynamicBoundaryKind> {
		match self {
			Scope::Function { .. } => Some(DynamicBoundaryKind::Function),
			Scope::Iteration { .. } => Some(DynamicBoundaryKind::Loop),
			_ => None,
		}
	}

	pub fn is_conditional(&self) -> bool {
		matches!(self, Scope::Conditional { .. })
	}
}

impl<'a> Environment<'a> {
	/// Handles all assignments, including updates and destructuring
	///
	/// Will evaluate the expression with the right timing and conditions, including never if short circuit
	///
	/// TODO finish operator. Unify increment and decrement. The RHS span should be fine with [`Span::NULL ...?`] Maybe RHS type could be None to accommodate
	pub fn assign_to_assignable_handle_errors<
		'b,
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
	>(
		&mut self,
		lhs: Assignable<A>,
		operator: AssignmentKind,
		// Can be `None` for increment and decrement
		expression: Option<&'b A::Expression<'b>>,
		assignment_span: Span,
		checking_data: &mut CheckingData<T, A>,
	) -> TypeId {
		match lhs {
			Assignable::Reference(reference) => {
				match operator {
					AssignmentKind::Assign => {
						let rhs = A::synthesise_expression(
							expression.unwrap(),
							TypeId::ANY_TYPE,
							self,
							checking_data,
						);

						self.assign_to_reference_assign_handle_errors(
							reference,
							rhs,
							checking_data,
							assignment_span,
						)
					}
					AssignmentKind::PureUpdate(operator) => {
						// Order matters here
						let reference_position = reference.get_position();
						let existing = self.get_reference(reference.clone(), checking_data, true);

						let expression = expression.unwrap();
						let expression_pos =
							A::expression_position(expression).with_source(self.get_source());
						let rhs = A::synthesise_expression(
							expression,
							TypeId::ANY_TYPE,
							self,
							checking_data,
						);

						let new = evaluate_pure_binary_operation_handle_errors(
							(existing, reference_position),
							operator.into(),
							(rhs, expression_pos),
							checking_data,
							self,
						);
						let result = self.set_reference(reference, new, checking_data);
						match result {
							Ok(ty) => ty,
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									self,
									error,
									assignment_span.with_source(self.get_source()),
									&checking_data.types,
									new,
								);
								checking_data.diagnostics_container.add_error(error);
								TypeId::ERROR_TYPE
							}
						}
					}
					AssignmentKind::IncrementOrDecrement(direction, return_kind) => {
						// let value =
						// 	self.get_variable_or_error(&name, &assignment_span, checking_data);
						let span = reference.get_position();
						let existing = self.get_reference(reference.clone(), checking_data, true);

						// TODO existing needs to be cast to number!!

						let new = evaluate_pure_binary_operation_handle_errors(
							(existing, span),
							match direction {
								crate::features::assignments::IncrementOrDecrement::Increment => {
									MathematicalAndBitwise::Add
								}
								crate::features::assignments::IncrementOrDecrement::Decrement => {
									MathematicalAndBitwise::Subtract
								}
							}
							.into(),
							(TypeId::ONE, source_map::Nullable::NULL),
							checking_data,
							self,
						);

						let result = self.set_reference(reference, new, checking_data);

						match result {
							Ok(new) => match return_kind {
								crate::features::assignments::AssignmentReturnStatus::Previous => {
									existing
								}
								crate::features::assignments::AssignmentReturnStatus::New => new,
							},
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									self,
									error,
									assignment_span.with_source(self.get_source()),
									&checking_data.types,
									new,
								);
								checking_data.diagnostics_container.add_error(error);
								TypeId::ERROR_TYPE
							}
						}
					}
					AssignmentKind::ConditionalUpdate(operator) => {
						let existing = self.get_reference(reference.clone(), checking_data, true);
						let expression = expression.unwrap();
						let new = evaluate_logical_operation_with_expression(
							(existing, reference.get_position().without_source()),
							operator,
							expression,
							checking_data,
							self,
						)
						.unwrap();

						let result = self.set_reference(reference, new, checking_data);

						match result {
							Ok(new) => new,
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									self,
									error,
									assignment_span.with_source(self.get_source()),
									&checking_data.types,
									new,
								);
								checking_data.diagnostics_container.add_error(error);
								TypeId::ERROR_TYPE
							}
						}
					}
				}
			}
			Assignable::ObjectDestructuring(assignments) => {
				debug_assert!(matches!(operator, AssignmentKind::Assign));

				let rhs = A::synthesise_expression(
					expression.unwrap(),
					TypeId::ANY_TYPE,
					self,
					checking_data,
				);

				self.assign_to_object_destructure_handle_errors(
					assignments,
					rhs,
					assignment_span,
					checking_data,
				)
			}
			Assignable::ArrayDestructuring(assignments) => {
				debug_assert!(matches!(operator, AssignmentKind::Assign));

				let rhs = A::synthesise_expression(
					expression.unwrap(),
					TypeId::ANY_TYPE,
					self,
					checking_data,
				);

				self.assign_to_array_destructure_handle_errors(
					assignments,
					rhs,
					assignment_span,
					checking_data,
				)
			}
		}
	}

	fn assign_to_reference_assign_handle_errors<
		'b,
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
	>(
		&mut self,
		reference: Reference,
		rhs: TypeId,
		checking_data: &mut CheckingData<'b, T, A>,
		assignment_span: source_map::BaseSpan<()>,
	) -> TypeId {
		let result = self.set_reference(reference, rhs, checking_data);

		match result {
			Ok(ty) => ty,
			Err(error) => {
				let error = set_property_error_to_type_check_error(
					self,
					error,
					assignment_span.with_source(self.get_source()),
					&checking_data.types,
					rhs,
				);
				checking_data.diagnostics_container.add_error(error);
				TypeId::ERROR_TYPE
			}
		}
	}

	fn assign_to_assign_only_handle_errors<
		'b,
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
	>(
		&mut self,
		lhs: Assignable<A>,
		rhs: TypeId,
		assignment_span: Span,
		checking_data: &mut CheckingData<T, A>,
	) -> TypeId {
		match lhs {
			Assignable::Reference(reference) => self.assign_to_reference_assign_handle_errors(
				reference,
				rhs,
				checking_data,
				assignment_span,
			),
			Assignable::ObjectDestructuring(assignments) => self
				.assign_to_object_destructure_handle_errors(
					assignments,
					rhs,
					assignment_span,
					checking_data,
				),
			Assignable::ArrayDestructuring(assignments) => self
				.assign_to_array_destructure_handle_errors(
					assignments,
					rhs,
					assignment_span,
					checking_data,
				),
		}
	}

	fn assign_to_object_destructure_handle_errors<
		'b,
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
	>(
		&mut self,
		assignments: Vec<AssignableObjectDestructuringField<A>>,
		rhs: TypeId,
		assignment_span: Span,
		checking_data: &mut CheckingData<'b, T, A>,
	) -> TypeId {
		for assignment in assignments {
			match assignment {
				AssignableObjectDestructuringField::Mapped {
					on,
					name,
					default_value,
					position,
				} => {
					let value = self.get_property(
						rhs,
						Publicity::Public,
						&on,
						&mut checking_data.types,
						None,
						position,
						&checking_data.options,
						false,
					);

					let rhs_value = if let Some((_, value)) = value {
						value
					} else if let Some(default_value) = default_value {
						A::synthesise_expression(
							default_value.as_ref(),
							TypeId::ANY_TYPE,
							self,
							checking_data,
						)
					} else {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::PropertyDoesNotExist {
								property: match on {
									PropertyKey::String(s) => {
										PropertyRepresentation::StringKey(s.to_string())
									}
									PropertyKey::Type(t) => PropertyRepresentation::Type(
										printing::print_type(t, &checking_data.types, self, false),
									),
								},
								on: TypeStringRepresentation::from_type_id(
									rhs,
									self,
									&checking_data.types,
									false,
								),
								site: position,
							},
						);

						TypeId::ERROR_TYPE
					};

					self.assign_to_assign_only_handle_errors(
						name,
						rhs_value,
						assignment_span,
						checking_data,
					);
				}
				AssignableObjectDestructuringField::Spread(_, _) => todo!(),
			}
		}

		rhs
	}

	fn assign_to_array_destructure_handle_errors<
		'b,
		T: crate::ReadFromFS,
		A: crate::ASTImplementation,
	>(
		&mut self,
		assignments: Vec<AssignableArrayDestructuringField<A>>,
		rhs: TypeId,
		assignment_span: Span,
		checking_data: &mut CheckingData<'b, T, A>,
	) -> TypeId {
		checking_data.raise_unimplemented_error(
			"destructuring array (needs iterator)",
			assignment_span.with_source(self.get_source()),
		);

		TypeId::ERROR_TYPE
	}

	fn get_reference<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		reference: Reference,
		checking_data: &mut CheckingData<U, A>,
		bind_this: bool,
	) -> TypeId {
		match reference {
			Reference::Variable(name, position) => {
				self.get_variable_handle_error(&name, position, checking_data).unwrap().1
			}
			Reference::Property { on, with, publicity, span } => {
				let get_property_handle_errors = self.get_property_handle_errors(
					on,
					publicity,
					&with,
					checking_data,
					span,
					bind_this,
				);
				match get_property_handle_errors {
					Ok(i) => i.get_value(),
					Err(()) => TypeId::ERROR_TYPE,
				}
			}
		}
	}

	fn set_reference<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		reference: Reference,
		rhs: TypeId,
		checking_data: &mut CheckingData<U, A>,
	) -> Result<TypeId, SetPropertyError> {
		match reference {
			Reference::Variable(name, position) => Ok(self.assign_to_variable_handle_errors(
				name.as_str(),
				position,
				rhs,
				checking_data,
			)),
			Reference::Property { on, with, publicity, span } => Ok(self
				.set_property(
					on,
					publicity,
					&with,
					rhs,
					&mut checking_data.types,
					Some(span),
					&checking_data.options,
				)?
				.unwrap_or(rhs)),
		}
	}

	pub fn assign_to_variable_handle_errors<T: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		variable_name: &str,
		assignment_position: SpanWithSource,
		new_type: TypeId,
		checking_data: &mut CheckingData<T, A>,
	) -> TypeId {
		let result = self.assign_to_variable(
			variable_name,
			assignment_position,
			new_type,
			&mut checking_data.types,
		);
		match result {
			Ok(ok) => ok,
			Err(error) => {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::AssignmentError(error));
				TypeId::ERROR_TYPE
			}
		}
	}

	/// This is top level variables, not properties.
	pub fn assign_to_variable(
		&mut self,
		variable_name: &str,
		assignment_position: SpanWithSource,
		new_type: TypeId,
		types: &mut TypeStore,
	) -> Result<TypeId, AssignmentError> {
		// Get without the effects
		let variable_in_map = self.get_variable_unbound(variable_name);

		if let Some((_, boundary, variable)) = variable_in_map {
			match variable {
				VariableOrImport::Variable { mutability, declared_at, context: _ } => {
					match mutability {
						VariableMutability::Constant => {
							Err(AssignmentError::Constant(*declared_at))
						}
						VariableMutability::Mutable { reassignment_constraint } => {
							let variable = variable.clone();
							let variable_site = *declared_at;
							let variable_id = variable.get_id();

							if boundary.is_none()
								&& !self.get_chain_of_info().any(|info| {
									info.variable_current_value.contains_key(&variable_id)
								}) {
								return Err(AssignmentError::TDZ(TDZ {
									position: assignment_position,
									variable_name: variable_name.to_owned(),
								}));
							}

							if let Some(reassignment_constraint) = *reassignment_constraint {
								// TODO tuple with position:
								let mut basic_subtyping = BasicEquality {
									add_property_restrictions: false,
									position: *declared_at,
									object_constraints: Default::default(),
									allow_errors: true,
								};

								let result = type_is_subtype(
									reassignment_constraint,
									new_type,
									&mut basic_subtyping,
									self,
									types,
								);

								self.add_object_constraints(
									basic_subtyping.object_constraints,
									types,
								);

								if let SubTypeResult::IsNotSubType(_mismatches) = result {
									return Err(AssignmentError::DoesNotMeetConstraint {
										variable_type: TypeStringRepresentation::from_type_id(
											reassignment_constraint,
											self,
											types,
											false,
										),
										value_type: TypeStringRepresentation::from_type_id(
											new_type, self, types, false,
										),
										variable_site,
										value_site: assignment_position,
									});
								}
							}

							self.info.events.push(Event::SetsVariable(
								variable_id,
								new_type,
								assignment_position,
							));
							self.info.variable_current_value.insert(variable_id, new_type);

							Ok(new_type)
						}
					}
				}
				VariableOrImport::MutableImport { .. }
				| VariableOrImport::ConstantImport { .. } => {
					Err(AssignmentError::Constant(assignment_position))
				}
			}
		} else {
			crate::utils::notify!("Could say it is on the window here");
			Err(AssignmentError::VariableNotFound {
				variable: variable_name.to_owned(),
				assignment_position,
			})
		}
	}

	pub(crate) fn get_root(&self) -> &RootContext {
		match self.context_type.parent {
			GeneralContext::Syntax(syntax) => syntax.get_root(),
			GeneralContext::Root(root) => root,
		}
	}

	#[must_use]
	pub fn get_environment_type(&self) -> &Scope {
		&self.context_type.scope
	}

	pub fn get_environment_type_mut(&mut self) -> &mut Scope {
		&mut self.context_type.scope
	}

	/// TODO decidable & private?
	#[must_use]
	pub fn property_in(&self, on: TypeId, property: &PropertyKey) -> bool {
		self.get_chain_of_info().any(|info| match info.current_properties.get(&on) {
			Some(v) => {
				v.iter().any(
					|(_, p, v)| if let PropertyValue::Deleted = v { false } else { p == property },
				)
			}
			None => false,
		})
	}

	/// TODO decidable & private?
	pub fn delete_property(&mut self, on: TypeId, property: &PropertyKey) -> bool {
		let existing = self.property_in(on, property);

		let under = property.into_owned();

		// on_default() okay because might be in a nested context.
		// entry empty does not mean no properties, just no properties set on this level
		self.info.current_properties.entry(on).or_default().push((
			Publicity::Public,
			under.clone(),
			PropertyValue::Deleted,
		));

		// TODO Event::Delete. Dependent result based on in
		self.info.events.push(Event::Setter {
			on,
			under,
			new: PropertyValue::Deleted,
			initialization: false,
			publicity: Publicity::Public,
			position: None,
		});

		existing
	}

	pub(crate) fn get_parent(&self) -> GeneralContext {
		match self.context_type.parent {
			GeneralContext::Syntax(syn) => GeneralContext::Syntax(syn),
			GeneralContext::Root(rt) => GeneralContext::Root(rt),
		}
	}

	#[allow(clippy::too_many_arguments)]
	pub fn get_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		property: &PropertyKey,
		types: &mut TypeStore,
		with: Option<TypeId>,
		position: SpanWithSource,
		options: &TypeCheckOptions,
		bind_this: bool,
	) -> Option<(PropertyKind, TypeId)> {
		crate::types::properties::get_property(
			on,
			publicity,
			property,
			with,
			self,
			&mut CheckThings { debug_types: options.debug_types },
			types,
			position,
			bind_this,
		)
	}

	pub fn get_property_handle_errors<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		key: &PropertyKey,
		checking_data: &mut CheckingData<U, A>,
		site: SpanWithSource,
		bind_this: bool,
	) -> Result<Instance, ()> {
		let get_property = self.get_property(
			on,
			publicity,
			key,
			&mut checking_data.types,
			None,
			site,
			&checking_data.options,
			bind_this,
		);

		if let Some((kind, result)) = get_property {
			Ok(match kind {
				PropertyKind::Getter => Instance::GValue(result),
				// TODO instance.property...?
				PropertyKind::Generic | PropertyKind::Direct => Instance::RValue(result),
			})
		} else {
			checking_data.diagnostics_container.add_error(TypeCheckError::PropertyDoesNotExist {
				// TODO printing temp
				property: match key {
					PropertyKey::String(s) => PropertyRepresentation::StringKey(s.to_string()),
					PropertyKey::Type(t) => PropertyRepresentation::Type(printing::print_type(
						*t,
						&checking_data.types,
						self,
						false,
					)),
				},
				on: crate::diagnostics::TypeStringRepresentation::from_type_id(
					on,
					self,
					&checking_data.types,
					false,
				),
				site,
			});
			Err(())
		}
	}

	pub fn get_variable_handle_error<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		name: &str,
		position: SpanWithSource,
		checking_data: &mut CheckingData<U, A>,
	) -> Result<VariableWithValue, TypeId> {
		let (in_root, crossed_boundary, og_var) = {
			let this = self.get_variable_unbound(name);
			if let Some((in_root, crossed_boundary, og_var)) = this {
				(in_root, crossed_boundary, og_var.clone())
			} else {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::CouldNotFindVariable {
						variable: name,
						// TODO
						possibles: Default::default(),
						position,
					},
				);
				return Err(TypeId::ERROR_TYPE);
			}
		};

		let reference = RootReference::Variable(og_var.get_id());

		if let VariableOrImport::Variable { context: Some(ref context), .. } = og_var {
			if let Some(ref current_context) = self.parents_iter().find_map(|a| {
				if let GeneralContext::Syntax(syn) = a {
					syn.context_type.location.clone()
				} else {
					None
				}
			}) {
				if current_context != context {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::VariableNotDefinedInContext {
							variable: name,
							expected_context: context,
							current_context: current_context.clone(),
							position,
						},
					);
					return Err(TypeId::ERROR_TYPE);
				}
			}
		}

		// let treat_as_in_same_scope = (og_var.is_constant && self.is_immutable(current_value));

		// TODO in_root temp fix
		if let (Some(_boundary), false) = (crossed_boundary, in_root) {
			let based_on = match og_var.get_mutability() {
				VariableMutability::Constant => {
					let constraint = checking_data
						.local_type_mappings
						.variables_to_constraints
						.0
						.get(&og_var.get_origin_variable_id());

					// TODO temp
					{
						let current_value = get_value_of_variable(
							self,
							og_var.get_id(),
							None::<&crate::types::poly_types::FunctionTypeArguments>,
						);

						if let Some(current_value) = current_value {
							let ty = checking_data.types.get_type_by_id(current_value);

							// TODO temp
							if matches!(ty, Type::SpecialObject(SpecialObjects::Function(..))) {
								return Ok(VariableWithValue(og_var.clone(), current_value));
							} else if let Type::RootPolyType(PolyNature::Open(_)) = ty {
								crate::utils::notify!(
									"Open poly type '{}' treated as immutable free variable",
									name
								);
								return Ok(VariableWithValue(og_var.clone(), current_value));
							} else if let Type::Constant(_) = ty {
								return Ok(VariableWithValue(og_var.clone(), current_value));
							}

							crate::utils::notify!("Free variable!");
						} else {
							crate::utils::notify!("No current value");
						}
					}

					// TODO is primitive, then can just use type
					if let Some(constraint) = constraint {
						*constraint
					} else {
						crate::utils::notify!("TODO record that parent variable is `any` here");
						TypeId::ANY_TYPE
					}
				}
				VariableMutability::Mutable { reassignment_constraint } => {
					// TODO is there a nicer way to do this
					// Look for reassignments
					for ctx in self.parents_iter() {
						if let Some(value) =
							get_on_ctx!(ctx.info.variable_current_value.get(&og_var.get_id()))
						{
							return Ok(VariableWithValue(og_var.clone(), *value));
						}
						let is_dynamic = matches!(ctx, GeneralContext::Syntax(s) if s.context_type.scope.is_dynamic_boundary().is_some());
						if is_dynamic {
							break;
						}
					}

					if let Some(constraint) = reassignment_constraint {
						constraint
					} else {
						crate::utils::notify!("TODO record that parent variable is `any` here");
						TypeId::ANY_TYPE
					}
				}
			};

			// TODO temp position
			let mut value = None;

			for event in &self.info.events {
				// TODO explain why don't need to detect sets
				if let Event::ReadsReference {
					reference: other_reference,
					reflects_dependency: Some(dep),
					position: _,
				} = event
				{
					if reference == *other_reference {
						value = Some(dep);
						break;
					}
				}
			}

			let type_id = if let Some(value) = value {
				*value
			} else {
				// TODO dynamic ?
				let ty = Type::RootPolyType(crate::types::PolyNature::FreeVariable {
					reference: reference.clone(),
					based_on,
				});
				let ty = checking_data.types.register_type(ty);

				// TODO would it be useful to record the type somewhere?
				self.context_type.free_variables.insert(reference);

				// if inferred {
				// 	self.context_type.get_inferrable_constraints_mut().unwrap().insert(type_id);
				// }

				self.info.events.push(Event::ReadsReference {
					reference: RootReference::Variable(og_var.get_id()),
					reflects_dependency: Some(ty),
					position,
				});

				ty
			};

			Ok(VariableWithValue(og_var.clone(), type_id))
		} else {
			// TODO recursively in
			if let VariableOrImport::MutableImport { of, constant: false, import_specified_at: _ } =
				og_var.clone()
			{
				let current_value = get_value_of_variable(
					self,
					of,
					None::<&crate::types::poly_types::FunctionTypeArguments>,
				)
				.expect("import not assigned yet");
				return Ok(VariableWithValue(og_var.clone(), current_value));
			}

			let current_value = get_value_of_variable(
				self,
				og_var.get_id(),
				None::<&crate::types::poly_types::FunctionTypeArguments>,
			);
			if let Some(current_value) = current_value {
				Ok(VariableWithValue(og_var.clone(), current_value))
			} else {
				checking_data.diagnostics_container.add_error(TypeCheckError::TDZ(TDZ {
					variable_name: self.get_variable_name(og_var.get_id()).to_owned(),
					position,
				}));
				Ok(VariableWithValue(og_var.clone(), TypeId::ERROR_TYPE))
			}
		}
	}

	pub fn new_conditional_context<T, A, R>(
		&mut self,
		(condition, pos): (TypeId, Span),
		then_evaluate: impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R,
		else_evaluate: Option<impl FnOnce(&mut Environment, &mut CheckingData<T, A>) -> R>,
		checking_data: &mut CheckingData<T, A>,
	) -> R
	where
		A: crate::ASTImplementation,
		R: TypeCombinable,
		T: crate::ReadFromFS,
	{
		if let Decidable::Known(result) = is_type_truthy_falsy(condition, &checking_data.types) {
			// TODO could be better
			checking_data.diagnostics_container.add_warning(TypeCheckWarning::DeadBranch {
				expression_span: pos.with_source(self.get_source()),
				expression_value: result,
			});

			return if result {
				then_evaluate(self, checking_data)
			} else if let Some(else_evaluate) = else_evaluate {
				else_evaluate(self, checking_data)
			} else {
				R::default()
			};
		}

		let (truthy_result, truthy_info, truthy_state) = {
			let mut truthy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: condition,
				is_switch: None,
			});
			let result = then_evaluate(&mut truthy_environment, checking_data);

			let Context {
				context_type: Syntax { free_variables, closed_over_references, state, .. },
				info,
				..
			} = truthy_environment;

			self.context_type.free_variables.extend(free_variables);
			self.context_type.closed_over_references.extend(closed_over_references);

			(result, info, state)
		};

		let (falsy_result, falsy_info, falsy_state) = if let Some(else_evaluate) = else_evaluate {
			let mut falsy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: checking_data.types.new_logical_negation_type(condition),
				is_switch: None,
			});

			let result = else_evaluate(&mut falsy_environment, checking_data);

			let Context {
				context_type: Syntax { free_variables, closed_over_references, state, .. },
				info,
				..
			} = falsy_environment;

			self.context_type.free_variables.extend(free_variables);
			self.context_type.closed_over_references.extend(closed_over_references);

			(result, Some(info), state)
		} else {
			(R::default(), None, ApplicationResult::Completed)
		};

		self.context_type.state =
			ApplicationResult::new_from_unknown_condition(condition, truthy_state, falsy_state);

		let combined_result =
			R::combine(condition, truthy_result, falsy_result, &mut checking_data.types);

		match self.context_type.parent {
			GeneralContext::Syntax(syn) => {
				merge_info(
					syn,
					&mut self.info,
					condition,
					truthy_info,
					falsy_info,
					&mut checking_data.types,
				);
			}
			GeneralContext::Root(root) => {
				merge_info(
					root,
					&mut self.info,
					condition,
					truthy_info,
					falsy_info,
					&mut checking_data.types,
				);
			}
		}

		combined_result
	}

	pub fn throw_value(&mut self, thrown: TypeId, position: SpanWithSource) {
		let final_event = FinalEvent::Throw { thrown, position };
		self.context_type.state.append_termination(final_event);
		self.info.events.push(final_event.into());
	}

	/// Also appends invalid return type checks
	pub fn return_value<T: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		expression: &Returnable<A>,
		checking_data: &mut CheckingData<T, A>,
	) {
		let expected = self.get_expected_return_type();
		let expected_split = expected.map(ExpectedReturnType::get_type_and_position);
		let expected_type = expected_split.map_or(TypeId::ANY_TYPE, |(l, _)| l);

		let (returned, returned_position) = match expression {
			Returnable::Statement(Some(expression), returned_position) => (
				A::synthesise_multiple_expression(expression, expected_type, self, checking_data),
				returned_position.with_source(self.get_source()),
			),
			Returnable::Statement(None, returned_position) => {
				(TypeId::UNDEFINED_TYPE, returned_position.with_source(self.get_source()))
			}
			Returnable::ArrowFunctionBody(expression) => (
				A::synthesise_expression(expression, expected_type, self, checking_data),
				A::expression_position(expression).with_source(self.get_source()),
			),
		};

		{
			// Don't check inferred, only annotations
			if let Some(ExpectedReturnType::FromReturnAnnotation(expected, position)) = expected {
				// TODO what about conditional things
				let mut basic_equality = BasicEquality {
					add_property_restrictions: false,
					position: source_map::Nullable::NULL,
					object_constraints: Default::default(),
					allow_errors: true,
				};

				let result = type_is_subtype(
					expected,
					returned,
					&mut basic_equality,
					self,
					&checking_data.types,
				);

				if let SubTypeResult::IsNotSubType(_) = result {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::ReturnedTypeDoesNotMatch {
							expected_return_type: TypeStringRepresentation::from_type_id(
								expected,
								self,
								&checking_data.types,
								checking_data.options.debug_types,
							),
							returned_type: TypeStringRepresentation::from_type_id(
								returned,
								self,
								&checking_data.types,
								checking_data.options.debug_types,
							),
							annotation_position: Some(position.with_source(self.get_source())),
							returned_position,
						},
					);
				}
			}
		}

		let final_event = FinalEvent::Return { returned, returned_position };
		self.context_type.state.append_termination(final_event);
		self.info.events.push(final_event.into());
	}

	pub fn add_continue(
		&mut self,
		label: Option<&str>,
		position: Span,
	) -> Result<(), NotInLoopOrCouldNotFindLabel> {
		if let Some(carry) = self.find_label_or_conditional_count(label, true) {
			self.info.events.push(
				FinalEvent::Continue {
					position: Some(position.with_source(self.get_source())),
					carry,
				}
				.into(),
			);
			Ok(())
		} else {
			Err(NotInLoopOrCouldNotFindLabel {
				label: label.map(ToOwned::to_owned),
				position: position.with_source(self.get_source()),
			})
		}
	}

	pub fn add_break(
		&mut self,
		label: Option<&str>,
		position: Span,
	) -> Result<(), NotInLoopOrCouldNotFindLabel> {
		if let Some(carry) = self.find_label_or_conditional_count(label, false) {
			self.info.events.push(
				FinalEvent::Break {
					position: Some(position.with_source(self.get_source())),
					carry,
				}
				.into(),
			);
			Ok(())
		} else {
			Err(NotInLoopOrCouldNotFindLabel {
				label: label.map(ToOwned::to_owned),
				position: position.with_source(self.get_source()),
			})
		}
	}

	/// Updates **a existing property**
	///
	/// Returns the result of the setter... TODO could return new else
	#[allow(clippy::too_many_arguments)]
	pub fn set_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		under: &PropertyKey,
		new: TypeId,
		types: &mut TypeStore,
		setter_position: Option<SpanWithSource>,
		options: &TypeCheckOptions,
	) -> Result<Option<TypeId>, SetPropertyError> {
		crate::types::properties::set_property(
			on,
			publicity,
			under,
			PropertyValue::Value(new),
			self,
			&mut CheckThings { debug_types: options.debug_types },
			types,
			setter_position,
		)
	}

	/// `continue` has different behavior to `break` right?
	fn find_label_or_conditional_count(
		&self,
		looking_for_label: Option<&str>,
		is_continue: bool,
	) -> Option<u8> {
		let mut falling_through_structures = 0;
		for ctx in self.parents_iter() {
			if let GeneralContext::Syntax(ctx) = ctx {
				let scope = &ctx.context_type.scope;

				match scope {
					Scope::Function(_)
					| Scope::InterfaceEnvironment { .. }
					| Scope::FunctionAnnotation {}
					| Scope::Module { .. }
					| Scope::DefinitionModule { .. }
					| Scope::TypeAlias
					| Scope::StaticBlock {} => {
						break;
					}
					Scope::Iteration { ref label } => {
						if looking_for_label.is_none() {
							return Some(falling_through_structures);
						} else if let Some(label) = label {
							if label == looking_for_label.unwrap() {
								return Some(falling_through_structures);
							}
						}
						falling_through_structures += 1;
					}
					Scope::Conditional { is_switch: Some(_label @ Some(_)), .. }
						if !is_continue && looking_for_label.is_some() =>
					{
						todo!("switch break")
					}
					Scope::PassThrough { .. }
					| Scope::Conditional { .. }
					| Scope::TryBlock {}
					| Scope::DefaultFunctionParameter {}
					| Scope::Block {} => {}
				}
			}
		}
		None
	}
}

fn set_property_error_to_type_check_error(
	ctx: &impl InformationChain,
	error: SetPropertyError,
	assignment_span: SpanWithSource,
	types: &TypeStore,
	new: TypeId,
) -> TypeCheckError<'static> {
	match error {
		SetPropertyError::NotWriteable => TypeCheckError::PropertyNotWriteable(assignment_span),
		SetPropertyError::DoesNotMeetConstraint { property_constraint, reason: _ } => {
			TypeCheckError::AssignmentError(AssignmentError::PropertyConstraint {
				property_constraint,
				value_type: TypeStringRepresentation::from_type_id(new, ctx, types, false),
				assignment_position: assignment_span,
			})
		}
	}
}

use source_map::{SourceId, Span, SpanWithSource};
use std::collections::HashSet;

use crate::{
	diagnostics::{
		NotInLoopOrCouldNotFindLabel, TypeCheckError, TypeCheckWarning, TypeStringRepresentation,
		TDZ,
	},
	events::{Event, FinalEvent, RootReference},
	features::{
		assignments::{Assignable, AssignmentKind, Reference},
		functions,
		modules::Exported,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, MathematicalAndBitwise,
		},
		variables::{VariableMutability, VariableOrImport, VariableWithValue},
	},
	subtyping::BasicEquality,
	types::{
		is_type_truthy_falsy,
		properties::{PropertyKey, PropertyKind, PropertyValue},
		subtyping::{type_is_subtype, SubTypeResult},
		PolyNature, Type, TypeCombinable, TypeStore,
	},
	CheckingData, Decidable, Instance, RootContext, TypeId,
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

	fn is_dynamic_boundary(&self) -> Option<DynamicBoundaryKind> {
		match &self.scope {
			Scope::Function { .. } => Some(DynamicBoundaryKind::Function),
			Scope::Iteration { .. } => Some(DynamicBoundaryKind::Loop),
			_ => None,
		}
	}

	fn is_conditional(&self) -> bool {
		matches!(self.scope, Scope::Conditional { .. })
	}

	fn get_closed_over_references(&mut self) -> Option<&mut ClosedOverReferencesInScope> {
		Some(&mut self.closed_over_references)
	}

	fn get_exports(&mut self) -> Option<&mut Exported> {
		if let Scope::Module { ref mut exported, .. } = self.scope {
			Some(exported)
		} else {
			None
		}
	}
}

/// TODO better names
/// Decides whether `await` and `yield` are available
#[derive(Debug, Clone)]
pub enum FunctionScope {
	ArrowFunction {
		// This always points to a poly free variable type
		free_this_type: TypeId,
		is_async: bool,
	},
	MethodFunction {
		// This always points to a poly free variable type
		free_this_type: TypeId,
		is_async: bool,
		is_generator: bool,
	},
	// is new-able
	Function {
		is_generator: bool,
		is_async: bool,
		// This always points to a conditional type based on `new.target === undefined`
		this_type: TypeId,
		type_of_super: TypeId,
	},
	Constructor {
		/// Can call `super`
		extends: bool,
		type_of_super: Option<TypeId>,
		// This is always creates, but may not be used (or have the relevant properties & prototype)
		this_object_type: TypeId,
	},
}

pub type Label = Option<String>;

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
		lhs: Assignable,
		operator: AssignmentKind,
		// Can be `None` for increment and decrement
		expression: Option<&'b A::Expression<'b>>,
		assignment_span: Span,
		checking_data: &mut CheckingData<T, A>,
	) -> TypeId {
		match lhs {
			Assignable::Reference(reference) => {
				/// Returns
				fn get_reference<U: crate::ReadFromFS, A: crate::ASTImplementation>(
					env: &mut Environment,
					reference: Reference,
					checking_data: &mut CheckingData<U, A>,
				) -> TypeId {
					match reference {
						Reference::Variable(name, position) => {
							env.get_variable_handle_error(&name, position, checking_data).unwrap().1
						}
						Reference::Property { on, with, publicity, span } => {
							let get_property_handle_errors = env.get_property_handle_errors(
								on,
								publicity,
								with,
								checking_data,
								span.without_source(),
							);
							match get_property_handle_errors {
								Ok(i) => i.get_value(),
								Err(()) => TypeId::ERROR_TYPE,
							}
						}
					}
				}

				fn set_reference<U: crate::ReadFromFS, A: crate::ASTImplementation>(
					env: &mut Environment,
					reference: Reference,
					new: TypeId,
					checking_data: &mut CheckingData<U, A>,
				) -> Result<TypeId, SetPropertyError> {
					match reference {
						Reference::Variable(name, position) => Ok(env
							.assign_to_variable_handle_errors(
								name.as_str(),
								position,
								new,
								checking_data,
							)),
						Reference::Property { on, with, publicity, span } => Ok(env
							.set_property(
								on,
								publicity,
								&with,
								new,
								&mut checking_data.types,
								Some(span),
							)?
							.unwrap_or(new)),
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
						SetPropertyError::NotWriteable => {
							TypeCheckError::PropertyNotWriteable(assignment_span)
						}
						SetPropertyError::DoesNotMeetConstraint {
							property_constraint,
							reason: _,
						} => TypeCheckError::AssignmentError(AssignmentError::PropertyConstraint {
							property_constraint,
							value_type: TypeStringRepresentation::from_type_id(
								new, ctx, types, false,
							),
							assignment_position: assignment_span,
						}),
					}
				}

				match operator {
					AssignmentKind::Assign => {
						let new = A::synthesise_expression(
							expression.unwrap(),
							TypeId::ANY_TYPE,
							self,
							checking_data,
						);
						let result = set_reference(self, reference, new, checking_data);
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
					AssignmentKind::PureUpdate(operator) => {
						// Order matters here
						let reference_position = reference.get_position();
						let existing = get_reference(self, reference.clone(), checking_data);

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
						let result = set_reference(self, reference, new, checking_data);
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
						let existing = get_reference(self, reference.clone(), checking_data);

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

						let result = set_reference(self, reference, new, checking_data);

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
						let existing = get_reference(self, reference.clone(), checking_data);
						let expression = expression.unwrap();
						let new = evaluate_logical_operation_with_expression(
							(existing, reference.get_position().without_source()),
							operator,
							expression,
							checking_data,
							self,
						)
						.unwrap();

						let result = set_reference(self, reference, new, checking_data);

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
			Assignable::ObjectDestructuring(_) => todo!(),
			Assignable::ArrayDestructuring(_) => todo!(),
		}
	}

	pub fn new_function<U, F, A>(
		&mut self,
		checking_data: &mut CheckingData<U, A>,
		function: &F,
		behavior: functions::FunctionRegisterBehavior<A>,
	) -> crate::types::FunctionType
	where
		U: crate::ReadFromFS,
		A: crate::ASTImplementation,
		F: functions::SynthesisableFunction<A>,
	{
		functions::register_function(self, behavior, function, checking_data)
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

		if let Some((_, _, variable)) = variable_in_map {
			match variable {
				VariableOrImport::Variable { mutability, declared_at, context: _ } => {
					match mutability {
						VariableMutability::Constant => {
							Err(AssignmentError::Constant(*declared_at))
						}
						VariableMutability::Mutable { reassignment_constraint } => {
							let variable = variable.clone();
							let variable_site = *declared_at;

							if let Some(reassignment_constraint) = *reassignment_constraint {
								// TODO tuple with position:
								let mut basic_subtyping = BasicEquality {
									add_property_restrictions: false,
									position: *declared_at,
									object_constraints: Default::default(),
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

							let variable_id = variable.get_id();

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

	pub fn get_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		property: PropertyKey,
		types: &mut TypeStore,
		with: Option<TypeId>,
		position: Span,
	) -> Option<(PropertyKind, TypeId)> {
		crate::types::properties::get_property(
			on,
			publicity,
			property,
			with,
			self,
			&mut CheckThings {},
			types,
			position.with_source(self.get_source()),
		)
	}

	pub fn get_property_handle_errors<U: crate::ReadFromFS, A: crate::ASTImplementation>(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		key: PropertyKey,
		checking_data: &mut CheckingData<U, A>,
		site: Span,
	) -> Result<Instance, ()> {
		let get_property =
			self.get_property(on, publicity, key.clone(), &mut checking_data.types, None, site);
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
					PropertyKey::String(s) => {
						crate::diagnostics::PropertyRepresentation::StringKey(s.to_string())
					}
					PropertyKey::Type(t) => crate::diagnostics::PropertyRepresentation::Type(
						crate::types::printing::print_type(t, &checking_data.types, self, false),
					),
				},
				on: crate::diagnostics::TypeStringRepresentation::from_type_id(
					on,
					self,
					&checking_data.types,
					false,
				),
				site: site.with_source(self.get_source()),
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
						.type_mappings
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
							if matches!(ty, Type::Function(..)) {
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
					for p in self.parents_iter() {
						if let Some(value) =
							get_on_ctx!(p.info.variable_current_value.get(&og_var.get_id()))
						{
							return Ok(VariableWithValue(og_var.clone(), *value));
						}
						if get_on_ctx!(p.context_type.is_dynamic_boundary()).is_some() {
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

		let (truthy_result, truthy_info) = {
			let mut truthy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: condition,
				is_switch: None,
			});

			let result = then_evaluate(&mut truthy_environment, checking_data);

			(result, truthy_environment.info)
		};

		let (falsy_result, falsy_info) = if let Some(else_evaluate) = else_evaluate {
			let mut falsy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: checking_data.types.new_logical_negation_type(condition),
				is_switch: None,
			});

			(else_evaluate(&mut falsy_environment, checking_data), Some(falsy_environment.info))
		} else {
			(R::default(), None)
		};

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
		self.info.events.push(FinalEvent::Throw { thrown, position }.into());
	}

	pub fn return_value(&mut self, returned: TypeId, returned_position: SpanWithSource) {
		self.info.events.push(FinalEvent::Return { returned, returned_position }.into());
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
			crate::utils::notify!("Carry is {}", carry);
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
	pub fn set_property(
		&mut self,
		on: TypeId,
		publicity: Publicity,
		under: &PropertyKey,
		new: TypeId,
		types: &mut TypeStore,
		setter_position: Option<SpanWithSource>,
	) -> Result<Option<TypeId>, SetPropertyError> {
		crate::types::properties::set_property(
			on,
			publicity,
			under,
			PropertyValue::Value(new),
			self,
			&mut CheckThings,
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

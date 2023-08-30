use source_map::Span;
use std::collections::HashMap;

use crate::{
	behavior::{
		assignments::{Assignable, AssignmentKind, Reference, SynthesizableExpression},
		operations::{
			evaluate_logical_operation, evaluate_pure_binary_operation_handle_errors,
			MathematicalAndBitwise,
		},
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	events::{Event, RootReference},
	structures::variables::{VariableMutability, VariableWithValue},
	subtyping::BasicEquality,
	types::{
		properties::{Property, PropertyResult},
		subtyping::{type_is_subtype, SubTypeResult},
		Type, TypeStore,
	},
	CheckingData, Root, TruthyFalsy, TypeId, VariableId,
};

use super::{AssignmentError, Context, ContextType, Environment, GeneralContext, SetPropertyError};

#[derive(Debug)]
pub struct Syntax<'a> {
	pub scope: Scope,
	pub(super) parent: GeneralContext<'a>,
	/// Synchronous events that occur
	pub events: Vec<Event>,

	/// Events that occur at any time
	///
	/// TODO maybe HashSet
	pub async_events: Vec<Event>,

	/// TODO rhs type is what...?
	pub closed_over_references: HashMap<RootReference, TypeId>,
	// /// Used for getting restrictions of closed over variables.
	// /// This should be empty at the end of synthesizing the scope
	// pub hoisted_variable_restrictions: HashMap<VariableId, TypeId>,
}

impl<'a> ContextType for Syntax<'a> {
	fn into_parent_or_root<'b>(et: &'b Context<Self>) -> GeneralContext<'b> {
		GeneralContext::Syntax(et)
	}

	fn get_parent<'b>(&'b self) -> Option<&'b GeneralContext<'b>> {
		Some(&self.parent)
	}

	fn is_dynamic_boundary(&self) -> bool {
		matches!(self.scope, Scope::Function { .. } | Scope::Looping { .. })
	}

	fn get_events(&mut self) -> Option<&mut Vec<Event>> {
		Some(&mut self.events)
	}
}

/// TODO name of structure
/// TODO conditionals should have conditional proofs (separate from the ones on context)
#[derive(Debug, Clone)]
pub enum Scope {
	Function {
		/// This also can be used to get the super type. Can be `any`
		this_constraint: TypeId,
		/// Aka can use `super`, todo not sure
		this_extends: bool,
		constructor_on: Option<TypeId>,
	},
	InterfaceEnvironment {
		this_constraint: TypeId,
	},
	ClassEnvironment {},
	FunctionReference {},
	/// For ifs, elses, or lazy operators
	Conditional {
		/// Things that are true
		antecedent: TypeId,
	},
	/// Variables here are dependent on the iteration,
	Looping {
		// TODO on: Proofs,
	},
	TryBlock {},
	// Just blocks and modules
	Block {},
	Module {},
}

impl<'a> Environment<'a> {
	/// Handles all assignments, including updates and destructuring
	///
	/// Will evaluate the expression with the right timing and conditions, including never if short circuit
	///
	/// TODO finish operator. Unify increment and decrement. The RHS span should be fine with Span::NULL ...? Maybe RHS type could be None to accommodate
	pub fn assign_to_assignable_handle_errors<U: crate::FSResolver>(
		&mut self,
		lhs: Assignable,
		operator: AssignmentKind,
		// Can be `None` for increment and decrement
		expression: Option<&impl SynthesizableExpression>,
		assignment_span: Span,
		checking_data: &mut CheckingData<U>,
	) -> TypeId {
		match lhs {
			Assignable::Reference(reference) => {
				/// Returns
				fn get_reference<U: crate::FSResolver>(
					env: &mut Environment,
					reference: Reference,
					checking_data: &mut CheckingData<U>,
				) -> TypeId {
					match reference {
						Reference::Variable(name, position) => {
							env.get_variable_or_error(&name, &position, checking_data).unwrap().1
						}
						Reference::Property { on, with, span } => {
							env.get_property_handle_errors(on, with, checking_data, span)
						}
					}
				}

				fn set_reference<U: crate::FSResolver>(
					env: &mut Environment,
					reference: Reference,
					new: TypeId,
					checking_data: &mut CheckingData<U>,
				) -> Result<TypeId, SetPropertyError> {
					match reference {
						Reference::Variable(name, position) => Ok(env
							.assign_to_variable_handle_errors(
								name.as_str(),
								position,
								new,
								checking_data,
							)),
						Reference::Property { on, with, span } => Ok(env
							.set_property(on, with, new, &mut checking_data.types)?
							.unwrap_or(new)),
					}
				}

				fn set_property_error_to_type_check_error(
					ctx: &GeneralContext,
					error: SetPropertyError,
					assignment_span: Span,
					types: &TypeStore,
					new: TypeId,
				) -> TypeCheckError<'static> {
					match error {
						SetPropertyError::NotWriteable => {
							TypeCheckError::PropertyNotWriteable(assignment_span)
						}
						SetPropertyError::DoesNotMeetConstraint(constraint, _) => {
							TypeCheckError::AssignmentError(AssignmentError::PropertyConstraint {
								property_type: TypeStringRepresentation::from_type_id(
									constraint, ctx, types, false,
								),
								value_type: TypeStringRepresentation::from_type_id(
									new, ctx, types, false,
								),
								assignment_position: assignment_span,
							})
						}
					}
				}

				match operator {
					AssignmentKind::Assign => {
						let new = expression.unwrap().synthesize_expression(self, checking_data);
						let result = set_reference(self, reference, new, checking_data);
						match result {
							Ok(ty) => ty,
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									&self.into_general_context(),
									error,
									assignment_span,
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
						let span = reference.get_position();
						let existing = get_reference(self, reference.clone(), checking_data);

						let expression = expression.unwrap();
						let rhs = (
							expression.synthesize_expression(self, checking_data),
							expression.get_position(),
						);
						let new = evaluate_pure_binary_operation_handle_errors(
							(existing, span),
							operator.into(),
							rhs,
							checking_data,
							self,
						);
						let result = set_reference(self, reference, new, checking_data);
						match result {
							Ok(ty) => ty,
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									&self.into_general_context(),
									error,
									assignment_span,
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
								crate::behavior::assignments::IncrementOrDecrement::Increment => {
									MathematicalAndBitwise::Add
								}
								crate::behavior::assignments::IncrementOrDecrement::Decrement => {
									MathematicalAndBitwise::Subtract
								}
							}
							.into(),
							(TypeId::ONE, Span::NULL_SPAN),
							checking_data,
							self,
						);

						let result = set_reference(self, reference, new, checking_data);

						match result {
							Ok(new) => match return_kind {
								crate::behavior::assignments::AssignmentReturnStatus::Previous => {
									existing
								}
								crate::behavior::assignments::AssignmentReturnStatus::New => new,
							},
							Err(error) => {
								let error = set_property_error_to_type_check_error(
									&self.into_general_context(),
									error,
									assignment_span,
									&checking_data.types,
									new,
								);
								checking_data.diagnostics_container.add_error(error);
								TypeId::ERROR_TYPE
							}
						}
					}
					AssignmentKind::ConditionalUpdate(operator) => {
						let _span = reference.get_position();
						let existing = get_reference(self, reference.clone(), checking_data);
						let expression = expression.unwrap();
						let new = evaluate_logical_operation(
							existing,
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
									&self.into_general_context(),
									error,
									assignment_span,
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

	pub fn assign_to_variable_handle_errors<T: crate::FSResolver>(
		&mut self,
		variable_name: &str,
		assignment_position: Span,
		new_type: TypeId,
		checking_data: &mut CheckingData<T>,
	) -> TypeId {
		let result = self.assign_to_variable(
			variable_name,
			assignment_position,
			new_type,
			&checking_data.types,
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
		assignment_position: Span,
		new_type: TypeId,
		store: &TypeStore,
	) -> Result<TypeId, AssignmentError> {
		// Get without the effects
		let variable_in_map = self.get_variable_unbound(variable_name);

		if let Some((_, variable)) = variable_in_map {
			match variable.mutability {
				VariableMutability::Constant => {
					return Err(AssignmentError::Constant(variable.declared_at.clone()));
				}
				VariableMutability::Mutable { reassignment_constraint } => {
					let variable = variable.clone();

					if let Some(reassignment_constraint) = reassignment_constraint {
						// TODO tuple with position:
						let mut basic_subtyping = BasicEquality {
							add_property_restrictions: false,
							position: variable.declared_at.clone(),
						};
						let result = type_is_subtype(
							reassignment_constraint,
							new_type,
							None,
							&mut basic_subtyping,
							self,
							store,
						);

						if let SubTypeResult::IsNotSubType(mismatches) = result {
							return Err(AssignmentError::DoesNotMeetConstraint {
								variable_type: TypeStringRepresentation::from_type_id(
									reassignment_constraint,
									&self.into_general_context(),
									store,
									false,
								),
								value_type: TypeStringRepresentation::from_type_id(
									new_type,
									&self.into_general_context(),
									store,
									false,
								),
								// TODO split
								variable_site: assignment_position.clone(),
								value_site: assignment_position.clone(),
							});
						}
					}

					let variable_id = variable.get_id();

					self.context_type
						.events
						.push(Event::SetsVariable(variable_id.clone(), new_type));
					self.variable_current_value.insert(variable_id, new_type);

					Ok(new_type)
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

	pub(crate) fn get_root(&self) -> &Root {
		match self.context_type.parent {
			GeneralContext::Syntax(syntax) => syntax.get_root(),
			GeneralContext::Root(root) => root,
		}
	}

	pub fn get_environment_id(&self) -> super::ContextId {
		self.context_id
	}

	pub(crate) fn get_environment_type(&self) -> &Scope {
		&self.context_type.scope
	}

	pub(crate) fn get_environment_type_mut(&mut self) -> &mut Scope {
		&mut self.context_type.scope
	}

	pub(crate) fn get_parent(&self) -> GeneralContext {
		match self.context_type.parent {
			GeneralContext::Syntax(syn) => GeneralContext::Syntax(syn),
			GeneralContext::Root(rt) => GeneralContext::Root(rt),
		}
	}

	/// Also evaluates getter and binds `this`
	pub fn get_property(
		&mut self,
		on: TypeId,
		property: TypeId,
		types: &mut TypeStore,
		with: Option<TypeId>,
	) -> Option<PropertyResult> {
		crate::types::properties::get_property(self, on, property, types, with)
	}

	pub fn get_property_handle_errors<U: crate::FSResolver>(
		&mut self,
		on: TypeId,
		property: TypeId,
		checking_data: &mut CheckingData<U>,
		site: Span,
	) -> TypeId {
		match self.get_property(on, property, &mut checking_data.types, None) {
			Some(ty) => ty.into(),
			None => {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::PropertyDoesNotExist {
						property: crate::diagnostics::TypeStringRepresentation::from_type_id(
							property,
							&self.into_general_context(),
							&checking_data.types,
							false,
						),
						on: crate::diagnostics::TypeStringRepresentation::from_type_id(
							on,
							&self.into_general_context(),
							&checking_data.types,
							false,
						),
						site,
					},
				);
				TypeId::ERROR_TYPE
			}
		}
	}

	pub fn get_variable_or_error<U: crate::FSResolver>(
		&mut self,
		name: &str,
		pos: &Span,
		checking_data: &mut CheckingData<U>,
	) -> Result<VariableWithValue, TypeId> {
		let (crossed_boundary, og_var) = {
			let this = self.get_variable_unbound(name);
			match this {
				Some((crossed_boundary, og_var)) => (crossed_boundary, og_var.clone()),
				None => {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::CouldNotFindVariable {
							variable: name,
							// TODO
							possibles: Default::default(),
							position: pos.clone(),
						},
					);
					return Err(TypeId::ERROR_TYPE);
				}
			}
		};

		let reference = RootReference::VariableId(og_var.get_id());
		// TODO
		// let treat_as_in_same_scope = (og_var.is_constant && self.is_immutable(current_value));

		if let Some(boundary) = crossed_boundary {
			let based_on = match og_var.mutability {
				VariableMutability::Constant => {
					let variable_id =
						VariableId(og_var.declared_at.source, og_var.declared_at.start);

					let constraint =
						checking_data.type_mappings.variables_to_constraints.0.get(&variable_id);

					let current_value = self.get_value_of_variable(og_var.get_id());

					if let Some(current_value) = current_value {
						let ty = checking_data.types.get_type_by_id(current_value);

						// TODO temp
						if matches!(ty, Type::Function(..)) {
							return Ok(VariableWithValue(og_var.clone(), current_value));
						}
					}

					// TODO is primitive, then can just use type
					match constraint {
						Some(constraint) => crate::types::PolyPointer::Fixed(*constraint),
						None => {
							checking_data.raise_unimplemented_error(
								"constraint across boundary",
								pos.clone(),
							);
							return Ok(VariableWithValue(
								og_var.clone(),
								TypeId::UNIMPLEMENTED_ERROR_TYPE,
							));
						}
					}
				}
				VariableMutability::Mutable { reassignment_constraint } => {
					match reassignment_constraint {
						Some(constraint) => crate::types::PolyPointer::Fixed(constraint),
						None => {
							checking_data.raise_unimplemented_error(
								"constraint across boundary",
								pos.clone(),
							);
							return Ok(VariableWithValue(
								og_var.clone(),
								TypeId::UNIMPLEMENTED_ERROR_TYPE,
							));
						}
					}
				}
			};

			// TODO temp position
			let mut value = None;
			for event in self.context_type.events.iter() {
				// TODO explain why don't need to detect sets
				if let Event::ReadsReference {
					reference: other_reference,
					reflects_dependency: Some(dep),
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
				// TODO
				let ty = Type::RootPolyType(crate::types::PolyNature::ParentScope {
					reference: reference.clone(),
					based_on,
				});
				let type_id = checking_data.types.register_type(ty);
				// TODO what is rhs
				self.context_type.closed_over_references.insert(reference, type_id);
				// if inferred {
				// 	self.context_type.get_inferrable_constraints_mut().unwrap().insert(type_id);
				// }

				self.context_type.events.push(Event::ReadsReference {
					reference: crate::events::RootReference::VariableId(og_var.get_id()),
					reflects_dependency: Some(type_id),
				});
				type_id
			};

			Ok(VariableWithValue(og_var.clone(), type_id))
		} else {
			let current_value =
				self.get_value_of_variable(og_var.get_id()).expect("variable not assigned yet");
			Ok(VariableWithValue(og_var.clone(), current_value))
		}
	}

	pub(crate) fn new_conditional_context<T: crate::FSResolver, U>(
		&mut self,
		condition: TypeId,
		then_evaluate: U,
		// TODO maybe V::Other
		else_evaluate: Option<U>,
		checking_data: &mut CheckingData<T>,
	) -> U::ExpressionResult
	where
		U: crate::SynthesizableConditional,
	{
		if let TruthyFalsy::Decidable(result) =
			self.is_type_truthy_falsy(condition, &checking_data.types)
		{
			// TODO emit warning
			return if result {
				then_evaluate.synthesize_condition(self, checking_data)
			} else if let Some(else_evaluate) = else_evaluate {
				else_evaluate.synthesize_condition(self, checking_data)
			} else {
				U::default_result()
			};
		}

		let (truthy_result, truthy_events) = {
			let mut truthy_environment =
				self.new_lexical_environment(Scope::Conditional { antecedent: condition });
			let result = then_evaluate.synthesize_condition(&mut truthy_environment, checking_data);
			(result, truthy_environment.context_type.events)
		};
		if let Some(else_evaluate) = else_evaluate {
			let mut falsy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: checking_data.types.new_logical_negation_type(condition),
			});
			let falsy_result =
				else_evaluate.synthesize_condition(&mut falsy_environment, checking_data);
			let falsy_events = falsy_environment.context_type.events;
			let combined_result = U::conditional_expression_result(
				condition,
				truthy_result,
				falsy_result,
				&mut checking_data.types,
			);
			// TODO
			self.context_type.events.push(Event::Conditionally {
				on: condition,
				events_if_truthy: truthy_events.into_boxed_slice(),
				else_events: falsy_events.into_boxed_slice(),
			});

			// TODO all things that are
			// - variable and property values (these aren't read from events)
			// - immutable, mutable, prototypes etc

			combined_result
		} else {
			self.context_type.events.push(Event::Conditionally {
				on: condition,
				events_if_truthy: truthy_events.into_boxed_slice(),
				else_events: Default::default(),
			});

			// TODO above

			truthy_result
		}
	}

	pub fn throw_value(&mut self, value: TypeId) {
		self.context_type.events.push(Event::Throw(value));
	}

	pub fn return_value(&mut self, returned: TypeId) {
		self.context_type.events.push(Event::Return { returned })
	}

	/// Updates **a existing property**
	///
	/// Returns the result of the setter... TODO could return new else
	pub fn set_property(
		&mut self,
		on: TypeId,
		under: TypeId,
		new: TypeId,
		types: &mut TypeStore,
	) -> Result<Option<TypeId>, SetPropertyError> {
		crate::types::properties::set_property(self, on, under, Property::Value(new), types)
	}

	/// Initializing
	pub fn create_property<U: crate::FSResolver>(
		&mut self,
		on: TypeId,
		under: TypeId,
		new: Property,
		checking_data: &mut CheckingData<U>,
	) {
		self.properties.entry(on).or_default().push((under, new));
	}

	pub(crate) fn create_this(&mut self, over: TypeId, store: &mut TypeStore) -> TypeId {
		let current_constructor_type = self.get_current_constructor().unwrap();

		let prototype = Some(over);

		// TODO I think
		let ty = store.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

		let condition = store.register_type(Type::Constructor(
			crate::types::Constructor::CanonicalRelationOperator {
				operator: crate::behavior::operations::CanonicalEqualityAndInequality::StrictEqual,
				lhs: TypeId::NEW_TARGET_ARG,
				rhs: current_constructor_type,
			},
		));

		let event = Event::Conditionally {
			on: condition,
			events_if_truthy: Box::new([Event::CreateObject {
				referenced_in_scope_as: ty,
				prototype,
			}]),
			else_events: Box::new([]),
		};

		self.context_type.get_events().unwrap().push(event);

		self.can_use_this = crate::context::CanUseThis::ConstructorCalled { this_ty: ty };
		ty
	}
}

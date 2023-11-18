use source_map::{SourceId, Span, SpanWithSource};
use std::collections::HashSet;

use crate::{
	behavior::{
		assignments::{Assignable, AssignmentKind, Reference},
		modules::{Exported, ImportKind, NamePair},
		objects::ObjectBuilder,
		operations::{
			evaluate_logical_operation_with_expression,
			evaluate_pure_binary_operation_handle_errors, MathematicalAndBitwise,
		},
		variables::{VariableMutability, VariableOrImport, VariableWithValue},
	},
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	events::{Event, RootReference},
	subtyping::BasicEquality,
	types::{
		is_type_truthy_falsy,
		properties::{PropertyKey, PropertyKind, PropertyValue},
		subtyping::{type_is_subtype, SubTypeResult},
		PolyNature, Type, TypeStore,
	},
	ASTImplementation, CheckingData, Instance, RootContext, TruthyFalsy, TypeCombinable, TypeId,
	VariableId,
};

use super::{
	calling::CheckThings,
	facts::{Facts, PublicityKind},
	get_value_of_variable, AssignmentError, ClosedOverReferencesInScope, Context, ContextType,
	Environment, GeneralContext, SetPropertyError,
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

	/// TODO WIP!
	pub location: ContextLocation,
}

impl<'a> ContextType for Syntax<'a> {
	fn as_general_context(et: &Context<Self>) -> GeneralContext<'_> {
		GeneralContext::Syntax(et)
	}

	fn get_parent(&self) -> Option<&GeneralContext<'_>> {
		Some(&self.parent)
	}

	fn is_dynamic_boundary(&self) -> bool {
		matches!(self.scope, Scope::Function { .. } | Scope::Looping { .. })
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

/// TODO name of structure
/// TODO conditionals should have conditional proofs (separate from the ones on context)
#[derive(Debug, Clone)]
pub enum Scope {
	Function(FunctionScope),
	InterfaceEnvironment {
		this_constraint: TypeId,
	},
	FunctionAnnotation {},
	/// For ifs, elses, or lazy operators
	Conditional {
		/// Something that is truthy for this to run
		antecedent: TypeId,
	},
	/// Variables here are dependent on the iteration,
	Looping {
		// TODO on: Proofs,
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
	/// TODO finish operator. Unify increment and decrement. The RHS span should be fine with Span::NULL ...? Maybe RHS type could be None to accommodate
	pub fn assign_to_assignable_handle_errors<U: crate::ReadFromFS, M: crate::ASTImplementation>(
		&mut self,
		lhs: Assignable,
		operator: AssignmentKind,
		// Can be `None` for increment and decrement
		expression: Option<&M::Expression>,
		assignment_span: SpanWithSource,
		checking_data: &mut CheckingData<U, M>,
	) -> TypeId {
		match lhs {
			Assignable::Reference(reference) => {
				/// Returns
				fn get_reference<U: crate::ReadFromFS, M: crate::ASTImplementation>(
					env: &mut Environment,
					reference: Reference,
					checking_data: &mut CheckingData<U, M>,
				) -> TypeId {
					match reference {
						Reference::Variable(name, position) => {
							env.get_variable_handle_error(&name, position.clone(), checking_data)
								.unwrap()
								.1
						}
						Reference::Property { on, with, publicity, span } => {
							let get_property_handle_errors = env.get_property_handle_errors(
								on,
								publicity,
								with,
								checking_data,
								span.clone(),
							);
							match get_property_handle_errors {
								Ok(i) => i.get_value(),
								Err(_) => TypeId::ERROR_TYPE,
							}
						}
					}
				}

				fn set_reference<U: crate::ReadFromFS, M: ASTImplementation>(
					env: &mut Environment,
					reference: Reference,
					new: TypeId,
					checking_data: &mut CheckingData<U, M>,
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
								with,
								new,
								&mut checking_data.types,
								Some(span),
							)?
							.unwrap_or(new)),
					}
				}

				fn set_property_error_to_type_check_error(
					ctx: &GeneralContext,
					error: SetPropertyError,
					assignment_span: SpanWithSource,
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
						let new = M::synthesise_expression(
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
									&self.as_general_context(),
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
						let reference_position = reference.get_position();
						let existing = get_reference(self, reference.clone(), checking_data);

						let expression = expression.unwrap();
						let expression_pos = M::expression_position(expression)
							.clone()
							.with_source(self.get_source());
						let rhs = M::synthesise_expression(
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
									&self.as_general_context(),
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
							(TypeId::ONE, SpanWithSource::NULL_SPAN),
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
									&self.as_general_context(),
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
						let new = evaluate_logical_operation_with_expression(
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
									&self.as_general_context(),
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

	pub fn assign_to_variable_handle_errors<T: crate::ReadFromFS, M: ASTImplementation>(
		&mut self,
		variable_name: &str,
		assignment_position: SpanWithSource,
		new_type: TypeId,
		checking_data: &mut CheckingData<T, M>,
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
		assignment_position: SpanWithSource,
		new_type: TypeId,
		store: &TypeStore,
	) -> Result<TypeId, AssignmentError> {
		// Get without the effects
		let variable_in_map = self.get_variable_unbound(variable_name);

		if let Some((_, _, variable)) = variable_in_map {
			match variable {
				VariableOrImport::Variable { mutability, declared_at, context } => {
					match mutability {
						VariableMutability::Constant => {
							Err(AssignmentError::Constant(declared_at.clone()))
						}
						VariableMutability::Mutable { reassignment_constraint } => {
							let variable = variable.clone();

							if let Some(reassignment_constraint) = reassignment_constraint.clone() {
								// TODO tuple with position:
								let mut basic_subtyping = BasicEquality {
									add_property_restrictions: false,
									position: declared_at.clone(),
								};
								let result = type_is_subtype(
									reassignment_constraint,
									new_type,
									&mut basic_subtyping,
									self,
									store,
								);

								if let SubTypeResult::IsNotSubType(mismatches) = result {
									return Err(AssignmentError::DoesNotMeetConstraint {
										variable_type: TypeStringRepresentation::from_type_id(
											reassignment_constraint,
											&self.as_general_context(),
											store,
											false,
										),
										value_type: TypeStringRepresentation::from_type_id(
											new_type,
											&self.as_general_context(),
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

							self.facts.events.push(Event::SetsVariable(
								variable_id,
								new_type,
								assignment_position,
							));
							self.facts.variable_current_value.insert(variable_id, new_type);

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

	pub fn get_environment_id(&self) -> super::ContextId {
		self.context_id
	}

	pub(crate) fn get_environment_type(&self) -> &Scope {
		&self.context_type.scope
	}

	/// TODO decidable & private?
	pub fn property_in(&self, on: TypeId, property: PropertyKey) -> bool {
		self.facts_chain().any(|facts| match facts.current_properties.get(&on) {
			Some(v) => {
				v.iter().any(
					|(_, p, v)| if let PropertyValue::Deleted = v { false } else { *p == property },
				)
			}
			None => false,
		})
	}

	/// TODO decidable & private?
	pub fn delete_property(&mut self, on: TypeId, property: PropertyKey) -> bool {
		let existing = self.property_in(on, property.clone());

		let under = property.into_owned();

		// on_default() okay because might be in a nested context.
		// entry empty does not mean no properties, just no properties set on this level
		self.facts.current_properties.entry(on).or_default().push((
			PublicityKind::Public,
			under.clone(),
			PropertyValue::Deleted,
		));

		// TODO Event::Delete. Dependent result based on in
		self.facts.events.push(Event::Setter {
			on,
			under,
			new: PropertyValue::Deleted,
			reflects_dependency: None,
			initialization: false,
			publicity: PublicityKind::Public,
			position: None,
		});

		existing
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
		publicity: PublicityKind,
		property: PropertyKey,
		types: &mut TypeStore,
		with: Option<TypeId>,
		position: SpanWithSource,
	) -> Option<(PropertyKind, TypeId)> {
		crate::types::properties::get_property(
			on,
			publicity,
			property,
			with,
			self,
			&mut CheckThings,
			types,
			position,
		)
	}

	pub fn get_property_handle_errors<U: crate::ReadFromFS, M: ASTImplementation>(
		&mut self,
		on: TypeId,
		publicity: PublicityKind,
		key: PropertyKey,
		checking_data: &mut CheckingData<U, M>,
		site: SpanWithSource,
	) -> Result<Instance, ()> {
		let get_property = self.get_property(
			on,
			publicity,
			key.clone(),
			&mut checking_data.types,
			None,
			site.clone(),
		);
		match get_property {
			Some((kind, result)) => Ok(match kind {
				PropertyKind::Getter => Instance::GValue(result),
				// TODO instance.property...?
				PropertyKind::Generic | PropertyKind::Direct => Instance::RValue(result),
			}),
			None => {
				let types = &checking_data.types;
				let ctx = &self.as_general_context();
				checking_data.diagnostics_container.add_error(
					TypeCheckError::PropertyDoesNotExist {
						// TODO printing temp
						property: match key {
							PropertyKey::String(s) => {
								crate::diagnostics::PropertyRepresentation::StringKey(s.to_string())
							}
							PropertyKey::Type(t) => {
								crate::diagnostics::PropertyRepresentation::Type(
									crate::types::printing::print_type(
										t,
										&checking_data.types,
										&self.as_general_context(),
										false,
									),
								)
							}
						},
						on: crate::diagnostics::TypeStringRepresentation::from_type_id(
							on, ctx, types, false,
						),
						site,
					},
				);
				Err(())
			}
		}
	}

	pub fn get_variable_handle_error<U: crate::ReadFromFS, M: ASTImplementation>(
		&mut self,
		name: &str,
		position: SpanWithSource,
		checking_data: &mut CheckingData<U, M>,
	) -> Result<VariableWithValue, TypeId> {
		let (in_root, crossed_boundary, og_var) = {
			let this = self.get_variable_unbound(name);
			match this {
				Some((in_root, crossed_boundary, og_var)) => {
					(in_root, crossed_boundary, og_var.clone())
				}
				None => {
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
		if let (Some(boundary), false) = (crossed_boundary, in_root) {
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
							self.facts_chain(),
							og_var.get_id(),
							None::<&crate::types::poly_types::FunctionTypeArguments>,
						);

						if let Some(current_value) = current_value {
							let ty = checking_data.types.get_type_by_id(current_value);

							// TODO temp
							if matches!(ty, Type::Function(..)) {
								return Ok(VariableWithValue(og_var.clone(), current_value));
							} else if let Type::RootPolyType(PolyNature::Open(ot)) = ty {
								crate::utils::notify!(
									"Open poly type treated as immutable free variable"
								);
								return Ok(VariableWithValue(og_var.clone(), *ot));
							} else {
								crate::utils::notify!("Free variable!");
							}
						}
					}

					// TODO is primitive, then can just use type
					match constraint {
						Some(constraint) => *constraint,
						None => {
							crate::utils::notify!("TODO record that parent variable is `any` here");
							TypeId::ANY_TYPE
						}
					}
				}
				VariableMutability::Mutable { reassignment_constraint } => {
					match reassignment_constraint {
						Some(constraint) => constraint,
						None => {
							crate::utils::notify!("TODO record that parent variable is `any` here");
							TypeId::ANY_TYPE
						}
					}
				}
			};

			// TODO temp position
			let mut value = None;

			for event in self.facts.events.iter() {
				// TODO explain why don't need to detect sets
				if let Event::ReadsReference {
					reference: other_reference,
					reflects_dependency: Some(dep),
					position,
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

				self.facts.events.push(Event::ReadsReference {
					reference: RootReference::Variable(og_var.get_id()),
					reflects_dependency: Some(ty),
					position,
				});

				ty
			};

			Ok(VariableWithValue(og_var.clone(), type_id))
		} else {
			// TODO recursively in
			if let VariableOrImport::MutableImport { of, constant: false, import_specified_at } =
				og_var.clone()
			{
				let current_value = get_value_of_variable(
					self.facts_chain(),
					of,
					None::<&crate::types::poly_types::FunctionTypeArguments>,
				)
				.expect("variable not assigned yet");
				return Ok(VariableWithValue(og_var.clone(), current_value));
			}

			let current_value = get_value_of_variable(
				self.facts_chain(),
				og_var.get_id(),
				None::<&crate::types::poly_types::FunctionTypeArguments>,
			)
			.expect("variable not assigned yet");
			Ok(VariableWithValue(og_var.clone(), current_value))
		}
	}

	pub(crate) fn new_conditional_context<T: crate::ReadFromFS, M, R>(
		&mut self,
		condition: TypeId,
		then_evaluate: impl FnOnce(&mut Environment, &mut CheckingData<T, M>) -> R,
		else_evaluate: Option<impl FnOnce(&mut Environment, &mut CheckingData<T, M>) -> R>,
		checking_data: &mut CheckingData<T, M>,
	) -> R
	where
		M: crate::ASTImplementation,
		R: TypeCombinable,
	{
		if let TruthyFalsy::Decidable(result) =
			is_type_truthy_falsy(condition, &checking_data.types)
		{
			// TODO emit warning
			return if result {
				then_evaluate(self, checking_data)
			} else if let Some(else_evaluate) = else_evaluate {
				else_evaluate(self, checking_data)
			} else {
				R::default()
			};
		}

		let (truthy_result, truthy_events) = {
			let mut truthy_environment =
				self.new_lexical_environment(Scope::Conditional { antecedent: condition });

			let result = then_evaluate(&mut truthy_environment, checking_data);

			(result, truthy_environment.facts.events)
		};

		if let Some(else_evaluate) = else_evaluate {
			let mut falsy_environment = self.new_lexical_environment(Scope::Conditional {
				antecedent: checking_data.types.new_logical_negation_type(condition),
			});

			let falsy_result = else_evaluate(&mut falsy_environment, checking_data);

			let combined_result =
				R::combine(condition, truthy_result, falsy_result, &mut checking_data.types);

			let falsy_events = falsy_environment.facts.events;
			// TODO It might be possible to get position from one of the SynthesisableConditional but its `get_position` is not implemented yet
			self.facts.events.push(Event::Conditionally {
				condition,
				events_if_truthy: truthy_events.into_boxed_slice(),
				else_events: falsy_events.into_boxed_slice(),
				position: None,
			});

			// TODO all things that are
			// - variable and property values (these aren't read from events)
			// - immutable, mutable, prototypes etc

			combined_result
		} else {
			self.facts.events.push(Event::Conditionally {
				condition,
				events_if_truthy: truthy_events.into_boxed_slice(),
				else_events: Default::default(),
				position: None,
			});

			// TODO above

			truthy_result
		}
	}

	pub fn throw_value(&mut self, value: TypeId, position: SpanWithSource) {
		self.facts.events.push(Event::Throw(value, position));
	}

	pub fn return_value(&mut self, returned: TypeId, returned_position: SpanWithSource) {
		self.facts.events.push(Event::Return { returned, returned_position })
	}

	/// Updates **a existing property**
	///
	/// Returns the result of the setter... TODO could return new else
	pub fn set_property(
		&mut self,
		on: TypeId,
		publicity: PublicityKind,
		under: PropertyKey,
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

	pub(crate) fn import_items<
		'b,
		P: Iterator<Item = NamePair<'b>>,
		T: crate::ReadFromFS,
		M: ASTImplementation,
	>(
		&mut self,
		partial_import_path: &str,
		import_position: Span,
		default_import: Option<(&str, Span)>,
		kind: ImportKind<'b, P>,
		checking_data: &mut CheckingData<T, M>,
		also_export: bool,
	) {
		let current_source = self.get_source();
		if !matches!(self.context_type.scope, crate::Scope::Module { .. }) {
			checking_data.diagnostics_container.add_error(TypeCheckError::NotTopLevelImport(
				import_position.with_source(current_source),
			));
			return;
		}

		let exports = checking_data.import_file(current_source, &partial_import_path, self);

		if let Err(ref err) = exports {
			checking_data.diagnostics_container.add_error(TypeCheckError::CannotOpenFile {
				file: err.clone(),
				position: Some(import_position.with_source(self.get_source())),
			});
		}

		if let Some((default_name, position)) = default_import {
			if let Ok(Ok(ref exports)) = exports {
				if let Some(item) = &exports.default {
					let id = crate::VariableId(current_source, position.start);
					let v = VariableOrImport::ConstantImport {
						to: None,
						import_specified_at: position.clone().with_source(current_source),
					};
					self.facts.variable_current_value.insert(id, *item);
					let existing = self.variables.insert(default_name.to_owned(), v);
					if let Some(existing) = existing {
						todo!("diagnostic")
					}
				} else {
					todo!("emit 'no default export' diagnostic")
				}
			} else {
				let behavior = crate::context::VariableRegisterBehavior::ConstantImport {
					value: TypeId::ERROR_TYPE,
				};

				self.register_variable_handle_error(
					default_name,
					position.with_source(current_source),
					behavior,
					checking_data,
				);
			}
		}

		match kind {
			ImportKind::Parts(parts) => {
				for part in parts {
					if let Ok(Ok(ref exports)) = exports {
						if let Some(export) = exports.get_export(part.value) {
							match export {
								crate::behavior::modules::TypeOrVariable::ExportedVariable((
									variable,
									mutability,
								)) => {
									let constant = match mutability {
										VariableMutability::Constant => {
											let k = crate::VariableId(
												current_source,
												part.position.start,
											);
											let v = self
												.get_value_of_constant_import_variable(variable);
											self.facts.variable_current_value.insert(k, v);
											true
										}
										VariableMutability::Mutable { reassignment_constraint } => {
											false
										}
									};

									let v = VariableOrImport::MutableImport {
										of: variable,
										constant,
										import_specified_at: part
											.position
											.clone()
											.with_source(self.get_source()),
									};
									let existing = self.variables.insert(part.r#as.to_owned(), v);
									if let Some(existing) = existing {
										todo!("diagnostic")
									}
									if also_export {
										if let Scope::Module { ref mut exported, .. } =
											self.context_type.scope
										{
											exported.named.push((
												part.r#as.to_owned(),
												(variable, mutability.clone()),
											));
										}
									}
								}
								crate::behavior::modules::TypeOrVariable::Type(ty) => {
									let existing =
										self.named_types.insert(part.r#as.to_owned(), ty);
									assert!(existing.is_none(), "TODO exception");
								}
							}
						} else {
							let position = part.position.with_source(current_source);
							checking_data.diagnostics_container.add_error(
								TypeCheckError::FieldNotExported {
									file: partial_import_path,
									position: position.clone(),
									importing: part.value,
								},
							);

							let behavior =
								crate::context::VariableRegisterBehavior::ConstantImport {
									value: TypeId::ERROR_TYPE,
								};

							self.register_variable_handle_error(
								part.r#as,
								position,
								behavior,
								checking_data,
							);
						}
					} else {
						let behavior = crate::context::VariableRegisterBehavior::ConstantImport {
							value: TypeId::ERROR_TYPE,
						};

						self.register_variable_handle_error(
							part.r#as,
							part.position.with_source(current_source),
							behavior,
							checking_data,
						);
					}
				}
			}
			ImportKind::All { under, position } => {
				if let Ok(Ok(ref exports)) = exports {
					let value = checking_data.types.register_type(Type::SpecialObject(
						crate::behavior::objects::SpecialObjects::Import(exports.clone()),
					));

					self.register_variable_handle_error(
						under,
						position.with_source(current_source),
						crate::context::VariableRegisterBehavior::ConstantImport { value },
						checking_data,
					);
				} else {
					let behavior = crate::context::VariableRegisterBehavior::Declare {
						base: TypeId::ERROR_TYPE,
						context: None,
					};
					self.register_variable_handle_error(
						under,
						position.with_source(current_source),
						behavior,
						checking_data,
					);
				}
			}
			ImportKind::Everything => {
				if let Ok(Ok(ref exports)) = exports {
					for (name, (variable, mutability)) in exports.named.iter() {
						// TODO are variables put into scope?
						if let Scope::Module { ref mut exported, .. } = self.context_type.scope {
							exported.named.push((name.clone(), (*variable, mutability.clone())));
						}
					}
				} else {
					// TODO ??
				}
			}
		}
	}
}

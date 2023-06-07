use std::collections::HashMap;

use source_map::Span;

use crate::{
	errors::TypeCheckError,
	events::{Event, Reference},
	structures::variables::{VariableMutability, VariableWithValue},
	subtyping::BasicEquality,
	types::{
		properties::PropertyResult,
		subtyping::{type_is_subtype, SubTypeResult},
		Type, TypeStore,
	},
	CheckingData, Root, TypeId,
};

use super::{
	Context, ContextType, Environment, GeneralEnvironment, ReassignmentError, SetPropertyError,
};

#[derive(Debug)]
pub struct Syntax<'a> {
	pub scope: Scope,
	pub(super) parent: GeneralEnvironment<'a>,
	/// Synchronous events that occur
	pub events: Vec<Event>,

	/// Events that occur at any time
	///
	/// TODO maybe HashSet
	pub async_events: Vec<Event>,

	/// TODO rhs type is what...?
	pub closed_over_variables: HashMap<Reference, TypeId>,
}

impl<'a> ContextType for Syntax<'a> {
	fn into_parent_or_root<'b>(et: &'b Context<Self>) -> GeneralEnvironment<'b> {
		GeneralEnvironment::Syntax(et)
	}

	fn get_parent<'b>(&'b self) -> Option<&'b GeneralEnvironment<'b>> {
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
		// TODO on: Proofs,
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
	pub fn assign_variable_handle_errors<T: crate::FSResolver>(
		&mut self,
		variable_name: &str,
		assignment_span: Span,
		new_type: TypeId,
		checking_data: &mut CheckingData<T>,
	) -> TypeId {
		let result = self.assign_variable(variable_name, new_type, &checking_data.types);
		match result {
			Ok(ok) => ok,
			Err(error) => {
				let error = match error {
					ReassignmentError::Constant(declared_at) => {
						TypeCheckError::CannotAssignToConstant {
							variable_name,
							variable_position: declared_at,
							assignment_position: assignment_span,
						}
					}
					ReassignmentError::VariableNotFound { variable } => {
						TypeCheckError::CouldNotFindVariable {
							variable: variable_name,
							possibles: Default::default(),
							position: assignment_span,
						}
					}
					ReassignmentError::DoesNotMatchRestrictionType {
						variable_type,
						variable_declared_at,
					} => TypeCheckError::InvalidAssignmentOrDeclaration {
						variable_type: crate::errors::TypeStringRepresentation::from_type_id(
							variable_type,
							&self.into_general_environment(),
							&checking_data.types,
							false,
						),
						value_type: crate::errors::TypeStringRepresentation::from_type_id(
							new_type,
							&self.into_general_environment(),
							&checking_data.types,
							false,
						),
						variable_site: variable_declared_at,
						// TODO not quite right
						value_site: assignment_span,
					},
				};
				checking_data.diagnostics_container.add_error(error);
				TypeId::ERROR_TYPE
			}
		}
	}

	/// This is top level variables, not properties (for now maybe).
	/// This is also for both updates and initial initialisation
	/// TODO check read has occurred
	///
	/// TODO Event is for...?
	/// TODO setters
	pub fn assign_variable<'b>(
		&mut self,
		variable_name: &'b str,
		new_type: TypeId,
		store: &TypeStore,
	) -> Result<TypeId, ReassignmentError<'b>> {
		// Get without the effects
		let variable_in_map = self.get_variable_unbound(variable_name);

		if let Some((_, variable)) = variable_in_map {
			match variable.mutability {
				VariableMutability::Constant => {
					return Err(ReassignmentError::Constant(variable.declared_at.clone()))
				}
				VariableMutability::Mutable { reassignment_constraint } => {
					let variable = variable.clone();
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
						return Err(ReassignmentError::DoesNotMatchRestrictionType {
							variable_declared_at: variable.declared_at.clone(),
							variable_type: reassignment_constraint,
						});
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
			Err(ReassignmentError::VariableNotFound { variable: variable_name })
		}
	}

	pub(crate) fn get_root(&self) -> &Root {
		match self.context_type.parent {
			GeneralEnvironment::Syntax(syntax) => syntax.get_root(),
			GeneralEnvironment::Root(root) => root,
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

	pub(crate) fn get_parent(&self) -> GeneralEnvironment {
		match self.context_type.parent {
			GeneralEnvironment::Syntax(syn) => GeneralEnvironment::Syntax(syn),
			GeneralEnvironment::Root(rt) => GeneralEnvironment::Root(rt),
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
		parent: TypeId,
		property: TypeId,
		checking_data: &mut CheckingData<U>,
		site: Span,
	) -> TypeId {
		match self.get_property(parent, property, &mut checking_data.types, None) {
			Some(ty) => ty.into(),
			None => {
				checking_data.diagnostics_container.add_error(
					TypeCheckError::PropertyDoesNotExist {
						property: crate::errors::TypeStringRepresentation::from_type_id(
							property,
							&self.into_general_environment(),
							&checking_data.types,
							false,
						),
						ty: crate::errors::TypeStringRepresentation::from_type_id(
							parent,
							&self.into_general_environment(),
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

	pub fn get_variable_or_error<U>(
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

		let reference = Reference::VariableId(og_var.get_id());
		// TODO
		// let treat_as_in_same_scope = (og_var.is_constant && self.is_immutable(current_value));

		let (value, reflects_dependency) = if let Some(boundary) = crossed_boundary {
			crate::utils::notify!("Found closed over type");

			// TODO
			let r#type = Type::RootPolyType(crate::types::PolyNature::ParentScope {
				reference: reference.clone(),
				based_on: crate::types::PolyPointer::Fixed(TypeId::ERROR_TYPE),
			});
			let type_id = checking_data.types.register_type(r#type);
			// TODO what is rhs
			self.context_type.closed_over_variables.insert(reference, type_id);
			// if inferred {
			// 	self.context_type.get_inferrable_constraints_mut().unwrap().insert(type_id);
			// }
			(VariableWithValue(og_var.clone(), type_id), Some(type_id))
		} else {
			let current_value = self.get_value_of_variable(og_var.get_id());
			(VariableWithValue(og_var.clone(), current_value), None)
		};

		self.context_type.events.push(Event::ReadsReference {
			reference: crate::events::Reference::VariableId(og_var.get_id()),
			reflects_dependency,
		});

		Ok(value)
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
		crate::types::properties::set_property(self, on, under, new, types)
	}

	/// Initializing
	pub fn create_property<U: crate::FSResolver>(
		&mut self,
		on: TypeId,
		under: TypeId,
		new: TypeId,
		checking_data: &mut CheckingData<U>,
	) {
		self.properties.entry(on).or_default().push((under, new));
	}

	pub(crate) fn create_this(&mut self, over: TypeId, store: &mut TypeStore) -> TypeId {
		let current_constructor_type = self.get_current_constructor().unwrap();

		let prototype = Some(over);

		// TODO I think
		let ty = store.register_type(Type::Object(crate::types::ObjectNature::RealDeal));

		let condition =
			store.register_type(Type::Constructor(crate::types::Constructor::RelationOperator {
				operator: crate::structures::operators::RelationOperator::Equal,
				lhs: TypeId::NEW_TARGET_ARG,
				rhs: current_constructor_type,
			}));

		let event = Event::Conditionally {
			on: condition,
			true_res: Box::new([Event::CreateObject { referenced_in_scope_as: ty, prototype }]),
			false_res: Box::new([]),
		};

		self.context_type.get_events().unwrap().push(event);
		self.can_use_this = crate::context::CanUseThis::ConstructorCalled { this_ty: ty };
		ty
	}
}

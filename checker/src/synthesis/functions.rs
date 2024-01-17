//! Function tings. Contains parameter synthesis, function body synthesis

use iterator_endiate::EndiateIteratorExt;
use parser::{
	expressions::ExpressionOrBlock, parameters::ParameterData, ASTNode, Block, FunctionBased,
	GenericTypeConstraint, TypeAnnotation, VariableField, VariableIdentifier, WithComment,
};
use source_map::{SourceId, SpanWithSource};

use crate::{
	context::{CanReferenceThis, Context, ContextType, Scope, VariableRegisterArguments},
	features::functions::{
		synthesise_function_default_value, FunctionBehavior, SynthesisableFunction,
	},
	types::poly_types::GenericTypeParameters,
	types::{
		functions::{SynthesisedParameter, SynthesisedParameters, SynthesisedRestParameter},
		poly_types::generic_type_arguments::TypeArgumentStore,
		FunctionType, StructureGenerics,
	},
	types::{Constructor, Type, TypeId},
	CheckingData, Environment, FunctionId,
};

use super::{
	expressions::synthesise_expression,
	synthesise_block,
	type_annotations::{comment_as_type_annotation, synthesise_type_annotation},
	variables::register_variable,
	Performs,
};

impl<U: FunctionBased + 'static> SynthesisableFunction<super::EznoParser>
	for parser::FunctionBase<U>
where
	U::Body: SynthesisableFunctionBody,
{
	fn id(&self, source_id: SourceId) -> FunctionId {
		FunctionId(source_id, self.get_position().start)
	}

	fn get_name(&self) -> Option<&str> {
		U::get_name(&self.name)
	}

	fn has_body(&self) -> bool {
		true
	}

	fn type_parameters<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<GenericTypeParameters> {
		self.type_parameters
			.as_ref()
			.map(|ty_params| synthesise_type_parameters(ty_params, environment, checking_data))
	}

	fn this_constraint<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<TypeId> {
		if let Some((ref annotation, _)) = self.parameters.this_type {
			crate::utils::notify!("Synthesising this restriction");
			Some(synthesise_type_annotation(annotation, environment, checking_data))
		} else {
			None
		}
	}

	fn super_constraint<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<TypeId> {
		if let Some((ref annotation, _)) = self.parameters.super_type {
			Some(synthesise_type_annotation(annotation, environment, checking_data))
		} else {
			None
		}
	}

	fn parameters<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
		expected_parameters: Option<&SynthesisedParameters>,
	) -> SynthesisedParameters {
		synthesise_function_parameters(
			&self.parameters,
			expected_parameters,
			environment,
			checking_data,
		)
	}

	fn return_type_annotation<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<(TypeId, SpanWithSource)> {
		self.return_type.as_ref().map(|reference| {
			(
				synthesise_type_annotation(reference, environment, checking_data),
				reference.get_position().with_source(environment.get_source()),
			)
		})
	}

	fn body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) {
		self.body.synthesise_function_body(environment, checking_data);
	}
}

pub(super) trait SynthesisableFunctionBody {
	// Return type is the return type of the body, if it doesn't use
	/// any returns it is equal to [`Type::Undefined`]
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	);
}

impl SynthesisableFunctionBody for Block {
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) {
		synthesise_block(&self.0, environment, checking_data);
	}
}

impl SynthesisableFunctionBody for ExpressionOrBlock {
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) {
		match self {
			ExpressionOrBlock::Expression(expression) => {
				// TODO expecting
				let returned =
					synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE);
				let position = expression.get_position().with_source(environment.get_source());
				environment.return_value(returned, position);
			}
			ExpressionOrBlock::Block(block) => {
				block.synthesise_function_body(environment, checking_data);
			}
		}
	}
}

pub(crate) fn synthesise_type_parameters<T: crate::ReadFromFS>(
	type_parameters: &[GenericTypeConstraint],
	environment: &mut crate::Environment,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
) -> GenericTypeParameters {
	type_parameters
		.iter()
		.map(|constraint| match constraint {
			GenericTypeConstraint::Parameter { name, default } => {
				let default_type = default
					.as_ref()
					.map(|ta| synthesise_type_annotation(ta, environment, checking_data));
				environment.new_explicit_type_parameter(
					name.as_str(),
					None,
					default_type,
					&mut checking_data.types,
				)
			}
			GenericTypeConstraint::Extends(name, extends) => {
				let extends = synthesise_type_annotation(extends, environment, checking_data);
				environment.new_explicit_type_parameter(
					name.as_str(),
					Some(extends),
					None,
					&mut checking_data.types,
				)
			}
			GenericTypeConstraint::ExtendsKeyOf(_, _) => todo!(),
			GenericTypeConstraint::Spread { name: _, default: _ } => todo!(),
		})
		.collect()
}

/// Returns the resolved type of parameters from a function definition
/// `expected_parameter_types` is for things like `.map(x => ..)` where for in the callback the type of `x`
/// can be elided and inferred from the type declaration of cb on map.
/// Only used for inference, does not check parameters meet function parameter type
///
/// Expected parameter types will be in the same order as the parameters
///
/// TODO reduce with other
pub(super) fn synthesise_type_annotation_function_parameters<T: crate::ReadFromFS>(
	reference_parameters: &parser::type_annotations::TypeAnnotationFunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> SynthesisedParameters {
	let parameters = reference_parameters
		.parameters
		.iter()
		.enumerate()
		.map(|(idx, parameter)| {
			let parameter_constraint =
				synthesise_type_annotation(&parameter.type_annotation, environment, checking_data);

			// TODO I think this is correct
			let parameter_constraint = if parameter.is_optional {
				checking_data.types.new_or_type(parameter_constraint, TypeId::UNDEFINED_TYPE)
			} else {
				parameter_constraint
			};

			let ty = checking_data.types.new_function_parameter(parameter_constraint);

			if let Some(name) = &parameter.name {
				register_variable(
					name.get_ast_ref(),
					environment,
					checking_data,
					VariableRegisterArguments {
						// TODO constant parameter option
						constant: false,
						space: Some(parameter_constraint),
						initial_value: Some(ty),
					},
				);
			};

			let name = parameter
				.name
				.as_ref()
				.map(WithComment::get_ast_ref)
				.map_or_else(|| format!("parameter{idx}"), get_parameter_name);

			SynthesisedParameter {
				ty,
				optional: parameter.is_optional,
				name,
				position: parameter.position.with_source(environment.get_source()),
			}
		})
		.collect();

	let rest_parameter = reference_parameters.rest_parameter.as_ref().map(|rest_parameter| {
		let parameter_constraint =
			synthesise_type_annotation(&rest_parameter.type_annotation, environment, checking_data);

		let item_type = if let TypeId::ERROR_TYPE = parameter_constraint {
			TypeId::ERROR_TYPE
		} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		})) = checking_data.types.get_type_by_id(parameter_constraint)
		{
			if let Some(item) = arguments.get_argument(TypeId::T_TYPE) {
				item
			} else {
				unreachable!()
			}
		} else {
			crate::utils::notify!("rest parameter should be array error");
			// checking_data.diagnostics_container.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};

		let ty = checking_data.types.new_function_parameter(parameter_constraint);

		environment.object_constraints.insert(ty, vec![parameter_constraint]);

		environment.register_variable_handle_error(
			&rest_parameter.name,
			VariableRegisterArguments {
				// TODO constant parameter option
				constant: false,
				space: Some(parameter_constraint),
				initial_value: Some(ty),
			},
			rest_parameter.position.with_source(environment.get_source()),
			&mut checking_data.diagnostics_container,
		);

		SynthesisedRestParameter {
			item_type,
			ty,
			name: rest_parameter.name.clone(),
			position: rest_parameter.position.with_source(environment.get_source()),
		}
	});

	SynthesisedParameters { parameters, rest_parameter }
}

fn synthesise_function_parameters<T: crate::ReadFromFS>(
	ast_parameters: &parser::FunctionParameters,
	expected_parameters: Option<&SynthesisedParameters>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> SynthesisedParameters {
	let parameters: Vec<_> = ast_parameters
		.parameters
		.iter()
		.enumerate()
		.map(|(idx, parameter)| {
			let parameter_constraint = parameter
				.type_annotation
				.as_ref()
				.map(|reference| synthesise_type_annotation(reference, environment, checking_data))
				.or_else(|| {
					// See comments-as-type-annotation
					if let WithComment::PostfixComment(_item, possible_declaration, position) =
						&parameter.name
					{
						comment_as_type_annotation(
							possible_declaration,
							&position.with_source(environment.get_source()),
							environment,
							checking_data,
						)
						.map(|(ty, _pos)| ty)
					} else {
						None
					}
				})
				.or_else(|| {
					// Try use expected type
					expected_parameters.as_ref().and_then(|p| p.get_type_constraint_at_index(idx))
				})
				.unwrap_or(TypeId::ANY_TYPE);

			// TODO I think this is correct
			let parameter_constraint = if let Some(ParameterData::Optional) = parameter.additionally
			{
				checking_data.types.new_or_type(parameter_constraint, TypeId::UNDEFINED_TYPE)
			} else {
				parameter_constraint
			};

			let ty = checking_data.types.new_function_parameter(parameter_constraint);

			// TODO parameter_constraint is stateless to reduce redundancy
			if !matches!(
				parameter_constraint,
				TypeId::NUMBER_TYPE | TypeId::STRING_TYPE | TypeId::BOOLEAN_TYPE
			) {
				environment.object_constraints.insert(ty, vec![parameter_constraint]);
			}

			let (optional, variable_ty) = match &parameter.additionally {
				Some(ParameterData::WithDefaultValue(expression)) => {
					let out = synthesise_function_default_value(
						ty,
						parameter_constraint,
						environment,
						checking_data,
						expression,
					);
					(true, out)
				}
				Some(ParameterData::Optional) => (true, ty),
				None => (false, ty),
			};

			register_variable(
				parameter.name.get_ast_ref(),
				environment,
				checking_data,
				VariableRegisterArguments {
					// TODO constant parameter option
					constant: false,
					space: Some(parameter_constraint),
					initial_value: Some(variable_ty),
				},
			);

			let name = param_name_to_string(parameter.name.get_ast_ref());

			SynthesisedParameter {
				name,
				optional,
				// Important != variable_ty here
				ty,
				position: parameter.position.with_source(environment.get_source()),
			}
		})
		.collect();

	let rest_parameter = ast_parameters.rest_parameter.as_ref().map(|rest_parameter| {
		// TODO should be Array<TypeId::ANY_TYPE>
		let parameter_constraint =
			rest_parameter.type_annotation.as_ref().map_or(TypeId::ANY_TYPE, |annotation| {
				synthesise_type_annotation(annotation, environment, checking_data)
			});

		let item_type = if let TypeId::ERROR_TYPE = parameter_constraint {
			TypeId::ERROR_TYPE
		} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		})) = checking_data.types.get_type_by_id(parameter_constraint)
		{
			if let Some(item) = arguments.get_argument(TypeId::T_TYPE) {
				item
			} else {
				unreachable!()
			}
		} else {
			crate::utils::notify!("rest parameter should be array error");
			// checking_data.diagnostics_container.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};

		let ty = checking_data.types.new_function_parameter(parameter_constraint);

		environment.object_constraints.insert(ty, vec![parameter_constraint]);

		match rest_parameter.name {
			VariableIdentifier::Standard(ref name, pos) => environment
				.register_variable_handle_error(
					name,
					VariableRegisterArguments {
						// TODO constant parameter option
						constant: false,
						space: Some(parameter_constraint),
						initial_value: Some(ty),
					},
					pos.with_source(environment.get_source()),
					&mut checking_data.diagnostics_container,
				),
			VariableIdentifier::Marker(_, _) => todo!(),
		};

		SynthesisedRestParameter {
			item_type,
			ty,
			name: rest_parameter.name.as_str().to_owned(),
			position: rest_parameter.position.with_source(environment.get_source()),
		}
	});

	SynthesisedParameters { parameters, rest_parameter }
}

fn param_name_to_string(param: &VariableField<parser::VariableFieldInSourceCode>) -> String {
	match param {
		VariableField::Name(name) => {
			if let VariableIdentifier::Standard(name, ..) = name {
				name.clone()
			} else {
				String::new()
			}
		}
		VariableField::Array(items, _) => {
			let mut buf = String::from("[");
			for (not_at_end, item) in items.iter().nendiate() {
				match item {
					parser::ArrayDestructuringField::Spread(_, name) => {
						buf.push_str("...");
						if let VariableIdentifier::Standard(name, ..) = name {
							buf.push_str(name);
						}
					}
					parser::ArrayDestructuringField::Name(name, _) => {
						buf.push_str(&param_name_to_string(name.get_ast_ref()));
					}
					parser::ArrayDestructuringField::None => {}
				}
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push(']');

			buf
		}
		VariableField::Object(items, _) => {
			let mut buf = String::from("{");
			for (not_at_end, item) in items.iter().nendiate() {
				match item.get_ast_ref() {
					parser::ObjectDestructuringField::Name(name, _, _) => {
						if let VariableIdentifier::Standard(name, ..) = name {
							buf.push_str(name);
						}
					}
					parser::ObjectDestructuringField::Spread(name, _) => {
						buf.push_str("...");
						if let VariableIdentifier::Standard(name, ..) = name {
							buf.push_str(name);
						}
					}
					parser::ObjectDestructuringField::Map { from, name, .. } => {
						match from {
							parser::PropertyKey::Ident(ident, _, ()) => {
								buf.push_str(ident);
							}
							parser::PropertyKey::StringLiteral(_, _, _) => todo!(),
							parser::PropertyKey::NumberLiteral(_, _) => todo!(),
							parser::PropertyKey::Computed(_, _) => todo!(),
						}
						buf.push_str(": ");
						buf.push_str(&param_name_to_string(name.get_ast_ref()));
					}
				}
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push_str(" }");

			buf
		}
	}
}

// TODO don't print values
fn get_parameter_name<T: parser::VariableFieldKind>(
	parameter: &parser::VariableField<T>,
) -> String {
	match parameter {
		VariableField::Name(name) => name.as_str().to_owned(),
		VariableField::Array(_items, _) => "todo".to_owned(),
		VariableField::Object(_, _) => "todo".to_owned(),
	}
}

/// This synthesise is for function types, references and interfaces.
///
/// TODO should always take effect annotations (right?)
///
/// TODO abstract
#[allow(clippy::too_many_arguments)]
pub(super) fn synthesise_function_annotation<T: crate::ReadFromFS, S: ContextType>(
	type_parameters: &Option<Vec<GenericTypeConstraint>>,
	parameters: &parser::type_annotations::TypeAnnotationFunctionParameters,
	// This Option rather than Option because function type references are always some
	return_type: Option<&TypeAnnotation>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	performs: super::Performs,
	position: &source_map::SpanWithSource,
	mut behavior: FunctionBehavior,
	on_interface: Option<TypeId>,
) -> FunctionType {
	// TODO don't have to create new environment if no generic type parameters or performs body
	environment
		.new_lexical_environment_fold_into_parent(
			Scope::FunctionAnnotation {},
			checking_data,
			|environment, checking_data| {
				match performs {
					Performs::Block(block) => {
						let new_scope = if let Some(on_interface) = on_interface {
							let free_this_type = checking_data.types.register_type(
								Type::RootPolyType(crate::types::PolyNature::FreeVariable {
									reference: crate::events::RootReference::This,
									based_on: on_interface,
								}),
							);
							if let FunctionBehavior::Method { ref mut free_this_id, .. } = behavior
							{
								*free_this_id = free_this_type;
							} else {
								unreachable!()
							}
							crate::context::environment::FunctionScope::MethodFunction {
								free_this_type,
								is_async: true,
								is_generator: true,
							}
						} else {
							crate::context::environment::FunctionScope::ArrowFunction {
								free_this_type: TypeId::ERROR_TYPE,
								is_async: true,
							}
						};
						let mut env =
							environment.new_lexical_environment(Scope::Function(new_scope));

						let type_parameters: Option<GenericTypeParameters> =
							type_parameters.as_ref().map(|type_parameters| {
								synthesise_type_parameters(type_parameters, &mut env, checking_data)
							});

						let parameters = synthesise_type_annotation_function_parameters(
							parameters,
							&mut env,
							checking_data,
						);

						let return_type =
							return_type.as_ref().map_or(TypeId::UNDEFINED_TYPE, |reference| {
								synthesise_type_annotation(reference, &mut env, checking_data)
							});

						env.can_reference_this = CanReferenceThis::Yeah;

						synthesise_block(&block.0, &mut env, checking_data);

						// TODO inject properties back
						FunctionType {
							// TODO
							id: FunctionId(position.source, position.start),
							parameters,
							return_type,
							type_parameters,
							effects: env.facts.events,
							free_variables: Default::default(),
							closed_over_variables: Default::default(),
							behavior,
							constant_function: None,
						}
					}
					other => {
						let type_parameters: Option<GenericTypeParameters> =
							type_parameters.as_ref().map(|type_parameters| {
								synthesise_type_parameters(
									type_parameters,
									environment,
									checking_data,
								)
							});

						let parameters = synthesise_type_annotation_function_parameters(
							parameters,
							environment,
							checking_data,
						);

						let return_type =
							return_type.as_ref().map_or(TypeId::UNDEFINED_TYPE, |reference| {
								synthesise_type_annotation(reference, environment, checking_data)
							});

						FunctionType {
							// TODO
							id: FunctionId(position.source, position.start),
							parameters,
							return_type,
							type_parameters,
							effects: Vec::new(),
							free_variables: Default::default(),
							closed_over_variables: Default::default(),
							behavior,
							constant_function: match other {
								Performs::Block(_) => unreachable!(),
								Performs::Const(id) => Some(id),
								Performs::None => None,
							},
						}
					}
				}
			},
		)
		.0
}

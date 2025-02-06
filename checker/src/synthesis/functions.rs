//! Function tings. Contains parameter synthesis, function body synthesis

use iterator_endiate::EndiateIteratorExt;
use parser::{
	expressions::ExpressionOrBlock,
	functions::{LeadingParameter, ParameterData},
	ASTNode, Block, FunctionBased, Span, SpreadDestructuringField, TypeAnnotation, TypeParameter,
	VariableField, VariableIdentifier, WithComment,
};

use crate::{
	context::{Context, ContextType, Scope, VariableRegisterArguments},
	features::functions::{synthesise_function_default_value, ReturnType},
	files::SynthesisableFunction,
	types::{
		functions::{
			FunctionBehavior, FunctionType, SynthesisedParameter, SynthesisedParameters,
			SynthesisedRestParameter,
		},
		generics::GenericTypeParameters,
		PartiallyAppliedGenerics, Type, TypeId,
	},
	CheckingData, Environment, FunctionId,
};

use super::{
	synthesise_block,
	type_annotations::{comment_as_type_annotation, synthesise_type_annotation},
	variables::register_variable,
};

impl<U: FunctionBased + 'static> SynthesisableFunction<super::EznoParser>
	for parser::FunctionBase<U>
where
	U::Body: SynthesisableFunctionBody,
{
	fn get_position(&self) -> Span {
		ASTNode::get_position(self)
	}

	fn get_name(&self) -> Option<&str> {
		U::get_name(&self.name)
	}

	fn has_body(&self) -> bool {
		self.body.is_some()
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
		if let Some(parser::functions::ThisParameter { constraint, .. }) =
			self.parameters.leading.get_this_parameter()
		{
			crate::utilities::notify!("Synthesising this restriction");
			Some(synthesise_type_annotation(constraint, environment, checking_data))
		} else {
			None
		}
	}

	fn super_constraint<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<TypeId> {
		if let Some(parser::functions::SuperParameter { constraint, .. }) =
			self.parameters.leading.get_super_parameter()
		{
			Some(synthesise_type_annotation(constraint, environment, checking_data))
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
	) -> Option<ReturnType> {
		self.return_type.as_ref().map(|reference| {
			ReturnType(
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
	/// Return type is the return type of the body, if it doesn't use
	/// any returns it is equal to [`Type::Undefined`]
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	);

	fn is_some(&self) -> bool;
}

impl SynthesisableFunctionBody for Block {
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) {
		synthesise_block(&self.0, environment, checking_data);
	}

	fn is_some(&self) -> bool {
		true
	}
}

impl SynthesisableFunctionBody for parser::functions::FunctionBody {
	fn synthesise_function_body<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) {
		self.0
			.as_ref()
			.expect("overload not caught")
			.synthesise_function_body(environment, checking_data);
	}

	fn is_some(&self) -> bool {
		self.0.is_some()
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
				let arrow_function_body =
					crate::context::environment::Returnable::ArrowFunctionBody(&**expression);
				environment.return_value(&arrow_function_body, checking_data);
			}
			ExpressionOrBlock::Block(block) => {
				block.synthesise_function_body(environment, checking_data);
			}
		}
	}

	fn is_some(&self) -> bool {
		true
	}
}

/// This also registers it to the environment
pub(crate) fn synthesise_type_parameters<T: crate::ReadFromFS>(
	type_parameters: &[TypeParameter],
	environment: &mut crate::Environment,
	checking_data: &mut crate::CheckingData<T, super::EznoParser>,
) -> GenericTypeParameters {
	type_parameters
		.iter()
		.map(|constraint| {
			let extends = constraint
				.extends
				.as_ref()
				.map(|extends| synthesise_type_annotation(extends, environment, checking_data));

			let default_type = constraint
				.default
				.as_ref()
				.map(|ta| synthesise_type_annotation(ta, environment, checking_data));

			environment.new_explicit_type_parameter(
				&constraint.name,
				extends,
				default_type,
				&mut checking_data.types,
			)
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

			// WIP
			let id = crate::VariableId(
				environment.get_source(),
				if let Some(parameter) = parameter.name.as_ref() {
					parameter.get_position().start
				} else {
					0
				},
			);
			let name = parameter
				.name
				.as_ref()
				.map(WithComment::get_ast_ref)
				.map_or_else(|| format!("parameter{idx}"), get_parameter_name);

			let ty = checking_data.types.new_function_parameter(parameter_constraint, id, &name);

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
						// :<)
						allow_reregistration: true,
					},
				);
			};

			SynthesisedParameter {
				ty,
				is_optional: parameter.is_optional,
				name,
				position: parameter.position.with_source(environment.get_source()),
			}
		})
		.collect();

	let rest_parameter = reference_parameters.rest_parameter.as_ref().map(|rest_parameter| {
		let parameter_constraint =
			synthesise_type_annotation(&rest_parameter.type_annotation, environment, checking_data);

		// WIP
		let id = crate::VariableId(environment.get_source(), rest_parameter.position.start);
		let name = &rest_parameter.name;
		let ty = checking_data.types.new_function_parameter(parameter_constraint, id, name);
		// environment.info.object_constraints.insert(ty, parameter_constraint);

		let item_type = if let TypeId::ERROR_TYPE = parameter_constraint {
			TypeId::ERROR_TYPE
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		}) = checking_data.types.get_type_by_id(parameter_constraint)
		{
			if let Some(item) = arguments.get_structure_restriction(TypeId::T_TYPE) {
				item
			} else {
				unreachable!()
			}
		} else {
			crate::utilities::notify!("rest parameter should be array error");
			// checking_data.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};

		environment.register_variable_handle_error(
			&rest_parameter.name,
			VariableRegisterArguments {
				// TODO constant parameter option
				constant: false,
				space: Some(parameter_constraint),
				initial_value: Some(ty),
				// :<)
				allow_reregistration: true,
			},
			rest_parameter.position.with_source(environment.get_source()),
			checking_data,
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

fn synthesise_function_parameters<
	T: crate::ReadFromFS,
	L: parser::functions::LeadingParameter,
	V: parser::functions::ParameterVisibility,
>(
	ast_parameters: &parser::functions::FunctionParameters<L, V>,
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
					expected_parameters
						.as_ref()
						.and_then(|p| p.get_parameter_type_at_index(idx).map(|(t, _pos)| t))
				})
				.unwrap_or(TypeId::ANY_TYPE);

			// TODO I think this is correct
			let parameter_constraint = if let Some(ParameterData::Optional) = parameter.additionally
			{
				checking_data.types.new_or_type(parameter_constraint, TypeId::UNDEFINED_TYPE)
			} else {
				parameter_constraint
			};

			let id =
				crate::VariableId(environment.get_source(), parameter.name.get_position().start);
			let name = variable_field_to_string(parameter.name.get_ast_ref());
			let ty = checking_data.types.new_function_parameter(parameter_constraint, id, &name);

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
					// :<)
					allow_reregistration: true,
				},
			);

			SynthesisedParameter {
				name,
				is_optional: optional,
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

		let id = crate::VariableId(environment.get_source(), rest_parameter.position.start);
		let name = variable_field_to_string(&rest_parameter.name);
		let ty = checking_data.types.new_function_parameter(parameter_constraint, id, &name);
		// environment.info.object_constraints.insert(variable_ty, parameter_constraint);

		let item_type = if let TypeId::ERROR_TYPE = parameter_constraint {
			TypeId::ERROR_TYPE
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		}) = checking_data.types.get_type_by_id(parameter_constraint)
		{
			if let Some(item) = arguments.get_structure_restriction(TypeId::T_TYPE) {
				item
			} else {
				unreachable!()
			}
		} else {
			crate::utilities::notify!("rest parameter should be array error");
			// checking_data.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};

		register_variable(
			&rest_parameter.name,
			environment,
			checking_data,
			VariableRegisterArguments {
				// TODO constant parameter option
				constant: false,
				space: Some(parameter_constraint),
				initial_value: Some(ty),
				// :<)
				allow_reregistration: true,
			},
		);

		let name = variable_field_to_string(&rest_parameter.name);

		SynthesisedRestParameter {
			item_type,
			ty,
			name,
			position: rest_parameter.position.with_source(environment.get_source()),
		}
	});

	SynthesisedParameters { parameters, rest_parameter }
}

/// For parameter printing
pub(super) fn variable_field_to_string(param: &VariableField) -> String {
	match param {
		VariableField::Name(name) => {
			if let VariableIdentifier::Standard(name, ..) = name {
				name.clone()
			} else {
				String::new()
			}
		}
		VariableField::Array { members, spread, .. } => {
			let mut buf = String::from("[");
			for (not_at_end, member) in members.iter().nendiate() {
				match member.get_ast_ref() {
					parser::ArrayDestructuringField::Name(name, ..) => {
						buf.push_str(&variable_field_to_string(name));
					}
					parser::ArrayDestructuringField::None => {}
				}
				if not_at_end {
					buf.push_str(", ");
				}
			}
			if let Some(SpreadDestructuringField(name, _)) = spread {
				if !members.is_empty() {
					buf.push_str(", ");
				}
				buf.push_str("...");
				buf.push_str(&variable_field_to_string(name));
			}
			buf.push(']');
			buf
		}
		VariableField::Object { members, spread, .. } => {
			let mut buf = String::from("{");
			for (not_at_end, item) in members.iter().nendiate() {
				match item.get_ast_ref() {
					parser::ObjectDestructuringField::Name(name, ..) => {
						if let VariableIdentifier::Standard(name, ..) = name {
							buf.push_str(name);
						}
					}
					parser::ObjectDestructuringField::Map { from, name, .. } => {
						match from {
							parser::PropertyKey::Identifier(ident, _, _) => {
								buf.push_str(ident);
							}
							parser::PropertyKey::StringLiteral(s, _, _) => {
								buf.push('"');
								buf.push_str(s);
								buf.push('"');
							}
							parser::PropertyKey::NumberLiteral(n, _) => {
								buf.push_str(n.clone().as_js_string().as_str());
							}
							parser::PropertyKey::Computed(_, _) => {
								// TODO maybe could do better here?
								buf.push_str("[...]");
							}
						}
						buf.push_str(": ");
						buf.push_str(&variable_field_to_string(name.get_ast_ref()));
					}
				}
				if not_at_end {
					buf.push_str(", ");
				}
			}
			if let Some(SpreadDestructuringField(name, _)) = spread {
				if !members.is_empty() {
					buf.push_str(", ");
				}
				buf.push_str("...");
				buf.push_str(&variable_field_to_string(name));
			}
			buf.push_str(" }");

			buf
		}
	}
}

// TODO don't print values
fn get_parameter_name(parameter: &parser::VariableField) -> String {
	match parameter {
		VariableField::Name(name) => match name {
			VariableIdentifier::Standard(ref name, _) => name.to_owned(),
			VariableIdentifier::Marker(_, _) => String::new(),
		},
		VariableField::Array { members: _, spread: _, position: _ } => {
			"todo: VariableField::Array".to_owned()
		}
		VariableField::Object { members: _, spread: _, position: _ } => {
			"todo: VariableField::Object".to_owned()
		}
	}
}

/// This synthesise is for function types, references and interfaces.
#[allow(clippy::too_many_arguments)]
pub(super) fn synthesise_function_annotation<T: crate::ReadFromFS, S: ContextType>(
	type_parameters: Option<&[TypeParameter]>,
	parameters: &parser::type_annotations::TypeAnnotationFunctionParameters,
	// This Option rather than Option because function type references are always some
	return_type: Option<&TypeAnnotation>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	position: &source_map::SpanWithSource,
	behavior: FunctionBehavior,
) -> FunctionType {
	// TODO don't have to create new environment if no generic type parameters or performs body
	environment
		.new_lexical_environment_fold_into_parent(
			Scope::FunctionAnnotation {},
			checking_data,
			|environment, checking_data| {
				let type_parameters: Option<GenericTypeParameters> =
					type_parameters.as_ref().map(|type_parameters| {
						synthesise_type_parameters(type_parameters, environment, checking_data)
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
					effect: crate::types::FunctionEffect::Unknown,
					behavior,
				}
			},
		)
		.0
}

/// For hoisting, don't have the events of the function
pub(super) fn synthesise_shape<T: crate::ReadFromFS, B: parser::FunctionBased>(
	function: &parser::FunctionBase<B>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> crate::features::functions::PartialFunction {
	environment
		.new_lexical_environment_fold_into_parent(
			Scope::FunctionAnnotation {},
			checking_data,
			|environment, checking_data| {
				let type_parameters = function.type_parameters.as_ref().map(|type_parameters| {
					super::functions::synthesise_type_parameters(
						type_parameters,
						environment,
						checking_data,
					)
				});

				let parameters = function
					.parameters
					.parameters
					.iter()
					.map(|parameter| {
						let parameter_constraint =
							parameter.type_annotation.as_ref().map_or(TypeId::ANY_TYPE, |ta| {
								synthesise_type_annotation(ta, environment, checking_data)
							});

						// TODO I think this is correct
						let is_optional = parameter.additionally.is_some();
						let ty = if is_optional {
							checking_data
								.types
								.new_or_type(parameter_constraint, TypeId::UNDEFINED_TYPE)
						} else {
							parameter_constraint
						};

						SynthesisedParameter {
							name: variable_field_to_string(parameter.name.get_ast_ref()),
							is_optional,
							ty,
							position: parameter.position.with_source(environment.get_source()),
						}
					})
					.collect();

				let rest_parameter =
					function.parameters.rest_parameter.as_ref().map(|rest_parameter| {
						let parameter_constraint = rest_parameter.type_annotation.as_ref().map_or(
							TypeId::ANY_TYPE,
							|annotation| {
								synthesise_type_annotation(annotation, environment, checking_data)
							},
						);

						let item_type = if let TypeId::ERROR_TYPE = parameter_constraint {
							TypeId::ERROR_TYPE
						} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
							on: TypeId::ARRAY_TYPE,
							arguments,
						}) = checking_data.types.get_type_by_id(parameter_constraint)
						{
							if let Some(item) = arguments.get_structure_restriction(TypeId::T_TYPE)
							{
								item
							} else {
								unreachable!()
							}
						} else {
							crate::utilities::notify!("rest parameter should be array error");
							// checking_data.add_error(
							// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
							// );
							TypeId::ERROR_TYPE
						};

						let name = variable_field_to_string(&rest_parameter.name);

						SynthesisedRestParameter {
							item_type,
							// This will be overridden when actual synthesis
							ty: parameter_constraint,
							name,
							position: rest_parameter.position.with_source(environment.get_source()),
						}
					});

				let return_type = function.return_type.as_ref().map(|annotation| {
					ReturnType(
						synthesise_type_annotation(annotation, environment, checking_data),
						annotation.get_position().with_source(environment.get_source()),
					)
				});

				crate::features::functions::PartialFunction(
					type_parameters,
					SynthesisedParameters { parameters, rest_parameter },
					return_type,
				)
			},
		)
		.0
}

/// TODO WIP
/// TODO also check generics?
#[allow(clippy::too_many_arguments)]
pub(super) fn build_overloaded_function<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	id: FunctionId,
	behavior: FunctionBehavior,
	overloads: Vec<crate::features::functions::PartialFunction>,
	actual: crate::features::functions::PartialFunction,
	environment: &Environment,
	checking_data: &mut crate::CheckingData<T, A>,
	effect: crate::types::FunctionEffect,
) -> TypeId {
	use crate::diagnostics::TypeCheckError;
	use crate::types::subtyping::{type_is_subtype, State, SubTypeResult};

	// TODO bad
	let expected_parameters = actual.1.clone();

	let as_function = FunctionType {
		id,
		behavior,
		type_parameters: actual.0,
		parameters: actual.1,
		return_type: actual.2.map_or(TypeId::ANY_TYPE, |rt| rt.0),
		effect,
	};

	let actual_func = checking_data.types.new_hoisted_function_type(as_function);

	let mut result = actual_func;

	for overload in overloads {
		for (idx, op) in overload.1.parameters.iter().enumerate() {
			if let Some((base_type, position)) =
				expected_parameters.get_parameter_type_at_index(idx)
			{
				let mut state = State {
					already_checked: Default::default(),
					mode: Default::default(),
					contributions: None,
					object_constraints: None,
					others: Default::default(),
				};
				let res = type_is_subtype(
					base_type,
					op.ty,
					&mut state,
					environment,
					&checking_data.types,
				);
				if let SubTypeResult::IsNotSubType(..) = res {
					let error = TypeCheckError::IncompatibleOverloadParameter {
						parameter_position: position,
						overloaded_parameter_position: op.position,
						parameter: base_type,
						overloaded_parameter: op.ty,
					};
					checking_data.add_error(error, environment);
				}
			} else {
				// TODO warning
			}
		}

		// TODO other cases
		if let (
			Some(ReturnType(base, base_position)),
			Some(ReturnType(overload, overload_position)),
		) = (actual.2, overload.2)
		{
			let mut state = State {
				already_checked: Default::default(),
				mode: Default::default(),
				contributions: None,
				object_constraints: None,
				others: Default::default(),
			};
			let res =
				type_is_subtype(base, overload, &mut state, environment, &checking_data.types);
			if let SubTypeResult::IsNotSubType(..) = res {
				let error = TypeCheckError::IncompatibleOverloadReturnType {
					base_position,
					overload_position,
					base,
					overload,
				};
				checking_data.add_error(error, environment);
			}
		} else {
			// TODO warning
		}

		// Partial
		let as_function = FunctionType {
			id,
			behavior,
			type_parameters: overload.0,
			parameters: overload.1,
			return_type: overload.2.map_or(TypeId::ANY_TYPE, |rt| rt.0),
			// existing is base?
			effect: crate::types::FunctionEffect::Unknown,
		};

		// TODO
		let func = checking_data.types.new_hoisted_function_type(as_function);

		// IMPORTANT THAT RESULT IS ON THE RIGHT OF AND TYPE
		result = checking_data.types.new_and_type(func, result);
	}

	result
}

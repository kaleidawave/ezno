//! Function tings. Contains parameter synthesis, function body synthesis

use std::{env, mem};

use parser::{
	expressions::ExpressionOrBlock, ASTNode, Block, FunctionBase, FunctionBased,
	GenericTypeConstraint, Statement, StatementOrDeclaration, TypeAnnotation, VariableField,
	VariableIdentifier, WithComment,
};
use source_map::{SourceId, Span, SpanWithSource};

use crate::{
	behavior::functions::{FunctionBehavior, SynthesisableFunction},
	context::{CanReferenceThis, Context, ContextType, Scope},
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
	expressions::synthesise_expression, hoisting::string_comment_to_type, synthesise_block,
	type_annotations::synthesise_type_annotation, variables::register_variable, Performs,
};

trait FunctionBasedItem: FunctionBased {
	type ObjectTypeId;

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind;

	fn location(func: &FunctionBase<Self>) -> Option<String> {
		None
	}
}

// TODO generic for these two
impl FunctionBasedItem for parser::functions::bases::StatementFunctionBase {
	type ObjectTypeId = ();

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	FunctionKind::StatementFunction {
	// 		is_async: func.header.is_async(),
	// 		generator: func.header.is_generator(),
	// 	}
	// }
}

impl FunctionBasedItem for parser::functions::bases::ExpressionFunctionBase {
	type ObjectTypeId = ();

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	FunctionKind::StatementFunction {
	// 		is_async: func.header.is_async(),
	// 		generator: func.header.is_generator(),
	// 	}
	// }

	fn location(func: &FunctionBase<Self>) -> Option<String> {
		match &func.header {
			parser::FunctionHeader::VirginFunctionHeader { location, .. }
			| parser::FunctionHeader::ChadFunctionHeader { location, .. } => {
				if let Some(parser::functions::FunctionLocationModifier::Server(_)) = location {
					Some("server".to_owned())
				} else {
					// if let Some(StatementOrDeclaration::Statement(Statement::Expression(expr))) =
					// 	func.body.0.first()
					// {
					// 	if matches!(expr, parser::expressions::MultipleExpression::Single(parser::Expression::StringLiteral(s, _, _)) if s == "use server")
					// 	{
					// 		return Some("server".to_owned());
					// 	}
					// }
					None
				}
			}
		}
	}
}

impl FunctionBasedItem for parser::functions::bases::ArrowFunctionBase {
	type ObjectTypeId = ();

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	let is_async = func.header.is_some();
	// 	FunctionKind::ArrowFunction { is_async }
	// }
}

impl FunctionBasedItem for parser::functions::bases::ObjectLiteralMethodBase {
	type ObjectTypeId = Option<TypeId>;

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	FunctionKind::Method(From::from(&func.header))
	// }
}

impl FunctionBasedItem for parser::functions::bases::ClassFunctionBase {
	type ObjectTypeId = Option<TypeId>;

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	FunctionKind::Method(From::from(&func.header))
	// }
}

impl FunctionBasedItem for parser::functions::bases::ClassConstructorBase {
	type ObjectTypeId = Option<TypeId>;

	// fn get_function_kind(func: &FunctionBase<Self>) -> FunctionKind {
	// 	FunctionKind::ClassConstructor
	// }
}

impl<U: FunctionBased + 'static> SynthesisableFunction<super::EznoParser>
	for parser::FunctionBase<U>
where
	U: FunctionBasedItem,
	U::Body: SynthesisableFunctionBody,
{
	fn id(&self, source_id: SourceId) -> FunctionId {
		FunctionId(source_id, self.get_position().start)
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
		expected_parameters: Option<SynthesisedParameters>,
	) -> SynthesisedParameters {
		synthesise_function_parameters(&self.parameters, environment, checking_data)
	}

	fn return_type_annotation<T: crate::ReadFromFS>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, super::EznoParser>,
	) -> Option<(TypeId, SpanWithSource)> {
		self.return_type.as_ref().map(|reference| {
			(
				synthesise_type_annotation(reference, environment, checking_data),
				reference.get_position().clone().with_source(environment.get_source()),
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
				let position =
					expression.get_position().clone().with_source(environment.get_source());
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
			GenericTypeConstraint::Spread { name, default } => todo!(),
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
			let parameter_type =
				synthesise_type_annotation(&parameter.type_annotation, environment, checking_data);

			// TODO temp for performs bodies
			let parameter_type = if let Some(name) = &parameter.name {
				register_variable(
					name.get_ast_ref(),
					environment,
					checking_data,
					crate::context::VariableRegisterBehavior::FunctionParameter {
						annotation: Some(parameter_type),
					},
					// TODO none...?
					Some(parameter_type),
				)
			} else {
				parameter_type
			};

			let name = parameter
				.name
				.as_ref()
				.map(WithComment::get_ast_ref)
				.map_or_else(|| format!("parameter{idx}"), get_parameter_name);

			let missing_value =
				if parameter.is_optional { Some(TypeId::UNDEFINED_TYPE) } else { None };

			SynthesisedParameter {
				ty: parameter_type,
				name,
				position: parameter.position.clone().with_source(environment.get_source()),
				missing_value,
			}
		})
		.collect();

	let rest_parameter = reference_parameters.rest_parameter.as_ref().map(|parameter| {
		let ty = synthesise_type_annotation(&parameter.type_annotation, environment, checking_data);
		let item_type = if let TypeId::ERROR_TYPE = ty {
			TypeId::ERROR_TYPE
		} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		})) = checking_data.types.get_type_by_id(ty)
		{
			if let Some(item) = arguments.get_argument(TypeId::T_TYPE) {
				item
			} else {
				unreachable!()
			}
		} else {
			todo!();
			// checking_data.diagnostics_container.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};
		SynthesisedRestParameter {
			item_type,
			name: parameter.name.clone(),
			position: parameter.position.clone().with_source(environment.get_source()),
		}
	});

	SynthesisedParameters { parameters, rest_parameter }
}

fn synthesise_function_parameters<T: crate::ReadFromFS>(
	ast_parameters: &parser::FunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> SynthesisedParameters {
	let parameters: Vec<_> = ast_parameters
		.parameters
		.iter()
		.map(|parameter| {
			let annotation = parameter
				.type_annotation
				.as_ref()
				.map(|reference| synthesise_type_annotation(reference, environment, checking_data))
				.or_else(|| {
					if let WithComment::PostfixComment(item, possible_declaration, position) =
						&parameter.name
					{
						string_comment_to_type(
							possible_declaration,
							&position.clone().with_source(environment.get_source()),
							environment,
							checking_data,
						)
						.map(|(ty, _pos)| ty)
					} else {
						None
					}
				});

			let param_type = register_variable(
				parameter.name.get_ast_ref(),
				environment,
				checking_data,
				crate::context::VariableRegisterBehavior::FunctionParameter { annotation },
				annotation,
			);
			let name = param_name_to_string(parameter.name.get_ast_ref());

			let missing_value = match &parameter.additionally {
				Some(parser::functions::ParameterData::Optional) => Some(TypeId::UNDEFINED_TYPE),
				Some(_expr) => todo!(),
				None => None,
			};

			SynthesisedParameter {
				name,
				ty: param_type,
				position: parameter.position.clone().with_source(environment.get_source()),
				missing_value,
			}
		})
		.collect();

	for parameter in &ast_parameters.rest_parameter {
		todo!()
		// super::variables::hoist_variable_identifier(&parameter.name, environment, is_constant);
	}
	SynthesisedParameters { parameters, rest_parameter: Default::default() }
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
		VariableField::Array(_, _) => todo!(),
		VariableField::Object(_, _) => todo!(),
	}
}

// TODO don't print values
fn get_parameter_name<T: parser::VariableFieldKind>(
	parameter: &parser::VariableField<T>,
) -> String {
	match parameter {
		VariableField::Name(name) => name.as_str().to_owned(),
		VariableField::Array(items, _) => "todo".to_owned(),
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

#[cfg(test)]
mod tests {

	use crate::{context::RootContext, TypeMappings};

	fn _get_base_environment() -> (RootContext, TypeMappings) {
		todo!()
		// let mut type_mappings = TypeMappings::default();
		// let environment = type_definition_file(
		//     TypeDefinitionModule::from_string(
		//         include_str!("..\\..\\definitions\\simple.d.ts").to_owned(),
		//         Default::default(),
		//         SourceId::new_null(),
		//         Vec::new(),
		//     )
		//     .unwrap(),
		//     &DiagnosticsContainer::default(),
		//     todo!()
		// );
		// (environment, type_mappings)
	}

	// TODO temp
	// #[test]
	// fn synthesis_test() {
	//     let function = "function add(x, y) {
	//         return x + y;
	//     }";
	//     let mut function = FunctionDeclaration::from_string(
	//         function.to_owned(),
	//         Default::default(),
	//         SourceId::new_null(),
	//         None,
	//     )
	//     .unwrap();
	//     let (environment, type_mappings) = get_base_environment();

	//     let function_type = synthesise_function(
	//         &function.base,
	//         &environment,
	//         &Default::default(),
	//         &type_mappings,
	//         None
	//     );

	//     todo!("{:#?}", function_type);
	// }
}

//! Function tings. Contains parameter synthesis, function body synthesis

use parser::{
	expressions::ExpressionOrBlock, ASTNode, Block, Chain, FunctionBase, FunctionBased,
	GenericTypeConstraint, TypeReference, VariableField, VariableIdentifier, WithComment,
};
use temporary_annex::Annex;

use crate::{
	context::{
		CanUseThis, {Context, ContextType, Scope, Syntax},
	},
	errors::{TypeCheckError, TypeStringRepresentation},
	events::Event,
	structures::{
		functions::{FunctionNature, FunctionType},
		parameters::{SynthesizedParameter, SynthesizedParameters, SynthesizedRestParameter},
	},
	types::poly_types::{
		type_generic_type_constraints, GenericFunctionTypeParameters, GenericTypeParameters,
	},
	types::{
		subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
		Constructor, PolyNature, Type, TypeId,
	},
	CheckingData, Environment,
};

use super::{synthesize_block, synthesize_expression, variables::synthesize_variable_field};

/// Returns the resolved type of parameters from a function definition
/// `expected_parameter_types` is for things like `.map(x => ..)` where for in the callback the type of `x`
/// can be elided and inferred from the type declaration of cb on map.
/// Only used for inference, does not check parameters meet function parameter type
///
/// Expected parameter types will be in the same order as the parameters
pub(crate) fn type_function_parameters_from_reference<T: crate::FSResolver>(
	reference_parameters: &parser::type_references::TypeReferenceFunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> SynthesizedParameters {
	let parameters = reference_parameters
		.parameters
		.iter()
		.enumerate()
		.map(|(idx, parameter)| {
			let parameter_type =
				environment.get_type_handle_errors(&parameter.type_reference, checking_data);

			SynthesizedParameter {
				ty: parameter_type,
				name: parameter
					.name
					.as_ref()
					.map(WithComment::get_ast)
					.map(get_parameter_name)
					.unwrap_or_else(|| format!("parameter{}", idx)),
				position: parameter.get_position().into_owned(),
			}
		})
		.collect();

	let optional_parameters = reference_parameters
		.optional_parameters
		.iter()
		.enumerate()
		.map(|(idx, parameter)| {
			let ty = environment.get_type_handle_errors(&parameter.type_reference, checking_data);

			let name = parameter
				.name
				.as_ref()
				.map(WithComment::get_ast)
				.map(get_parameter_name)
				.unwrap_or_else(|| format!("parameter{}", idx));

			SynthesizedParameter { ty, name, position: parameter.get_position().into_owned() }
		})
		.collect();

	let rest_parameter = reference_parameters.rest_parameter.as_ref().map(|rest_parameter| {
		let ty = environment.get_type_handle_errors(&rest_parameter.type_reference, checking_data);
		let item_type = if let TypeId::ERROR_TYPE = ty {
			TypeId::ERROR_TYPE
		} else if let Type::Constructor(Constructor::StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			with,
		}) = checking_data.types.get_type_by_id(ty)
		{
			with[&TypeId::T_TYPE]
		} else {
			todo!();
			// checking_data.diagnostics_container.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};
		SynthesizedRestParameter {
			item_type,
			name: rest_parameter.name.clone(),
			position: rest_parameter.type_reference.get_position().into_owned(),
		}
	});

	SynthesizedParameters { parameters, optional_parameters, rest_parameter }
}

fn hoist_function_parameters(
	parameters: &parser::FunctionParameters,
	environment: &mut Environment,
) {
	let is_constant = false;
	for parameter in parameters.parameters.iter() {
		super::variables::hoist_variable_declaration(
			parameter.name.get_ast(),
			environment,
			is_constant,
		);
	}
	for parameter in parameters.optional_parameters.iter() {
		match parameter {
			parser::OptionalOrWithDefaultValueParameter::Optional { name, type_reference } => {
				todo!()
			}
			parser::OptionalOrWithDefaultValueParameter::WithDefaultValue {
				name,
				type_reference,
				value,
			} => super::variables::hoist_variable_declaration(
				name.get_ast(),
				environment,
				is_constant,
			),
		}
	}
	for parameter in parameters.rest_parameter.iter() {
		super::variables::hoist_variable_identifier(&parameter.name, environment, is_constant);
	}
}

pub(crate) fn type_function_parameters<T: crate::FSResolver>(
	ast_parameters: &mut parser::FunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> SynthesizedParameters {
	hoist_function_parameters(&ast_parameters, environment);

	let parameters = ast_parameters
		.parameters
		.iter_mut()
		.map(|parameter| {
			synthesize_function_parameter(parameter, environment, checking_data, chain)
		})
		.collect();

	let optional_parameters = ast_parameters
		.optional_parameters
		.iter_mut()
		.map(|parameter| match parameter {
			parser::OptionalOrWithDefaultValueParameter::Optional { name, type_reference } => {
				let VariableIdentifier::Standard(name, variable_id, position) = name else { panic!() };

				let parameter_type =
					synthesize_parameter_type_reference(type_reference, environment, checking_data);

				environment
					.register_variable(
						name,
						*variable_id,
						position.clone(),
						crate::context::VariableRegisterBehavior::FunctionParameter {
							base: parameter_type,
						},
						&mut checking_data.types,
					)
					.expect("TODO duplicate parameter names");

				// TODO in parser
				let span = position.clone();
				let position = if let Some(type_reference) = type_reference {
					span.union(&type_reference.get_position())
				} else {
					span
				};

				SynthesizedParameter { ty: parameter_type, name: name.clone(), position }
			}
			parser::OptionalOrWithDefaultValueParameter::WithDefaultValue {
				name,
				type_reference,
				value,
			} => {
				// let parameter_type =
				// 	synthesize_parameter_type_reference(type_reference, environment, checking_data);

				// let default_value_type = environment.new_lexical_environment_fold(
				// 	Scope::Conditional { on: crate::proofs::Proofs::default() },
				// 	|environment| synthesize_expression(value, environment, checking_data, chain),
				// );

				// crate::utils::notify!("TODO check default against parameter type constraint here");
				todo!();
				// let union_type = TypeId::Union(vec![parameter_type.clone(), default_value_type]);
				// synthesize_variable_field(
				//     name.get_ast_mut(),
				//     None,
				//     &union_type,
				//     checking_data.settings.constant_parameters,
				//     environment,
				//     checking_data,
				//     chain,
				// );
				// SynthesizedParameter(
				//     Some(parameter_type),
				//     get_variable_field_name(name.get_ast()),
				//     position.clone(),
				// )
			}
		})
		.collect();

	let rest_parameter = if let Some(ref spread_parameter) = ast_parameters.rest_parameter {
		todo!()
	// let (rest_array_parameter_type, array_inside_type) =
	// if spread_parameter.type_reference.is_some() {
	// 	let reference = synthesize_parameter_type_reference(
	// 		&spread_parameter.type_reference,
	// 		environment,
	// 		checking_data,
	// 		constraints,
	// 	);
	// 	// let inside_type: Option<TypeId> =
	// 	//     if let TypeId::SpecializedGeneric(SpecializedGeneric {
	// 	//         generic_type: GenericInterfaceTypeId::ARRAY_TYPE,
	// 	//         arguments,
	// 	//     }) = reference
	// 	//     {
	// 	//         todo!()
	// 	//         // arguments.into_iter().next().unwrap().1
	// 	//     } else {
	// 	//         todo!("error function with non array based thingy")
	// 	//     };
	// 	// crate::utils::notify!("check type is array here");
	// 	// (reference, inside_type)
	// } else {
	// 	// ArrayType(Type::new_inferred_generic_type_parameter(constraints)).get_known_type(
	// 	//     &checking_data.memory,
	// 	//     &mut GetTypeFromReferenceSettings::SourceParameterPosition(constraints),
	// 	// )
	// };
	// environment.declare_variable(
	//     &spread_parameter.name,
	//     None,
	//     rest_array_parameter_type,
	//     spread_parameter.variable_id,
	//     checking_data.settings.constant_parameters,
	//     spread_parameter.position.clone(),
	// );
	// Some(Box::new(SynthesizedRestParameter(
	//     array_inside_type,
	//     spread_parameter.name.clone(),
	//     spread_parameter.position.clone(),
	// )))
	} else {
		None
	};

	SynthesizedParameters { rest_parameter, optional_parameters, parameters }
}

fn synthesize_function_parameter<T: crate::FSResolver>(
	parameter: &mut parser::Parameter,
	environment: &mut Context<Syntax>,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> SynthesizedParameter {
	let parameter_type =
		synthesize_parameter_type_reference(&parameter.type_reference, environment, checking_data);

	synthesize_variable_field(
		parameter.name.get_ast_mut(),
		None,
		parameter_type,
		checking_data.settings.constant_parameters,
		environment,
		checking_data,
		chain,
	);

	let position = parameter.get_position().into_owned();

	SynthesizedParameter {
		ty: parameter_type,
		name: get_parameter_name(&parameter.name.get_ast()),
		position,
	}
}

// TODO don't print values
fn get_parameter_name<T: parser::VariableFieldTypes>(
	parameter: &parser::VariableField<T>,
) -> String {
	match parameter {
		VariableField::Name(name) => name.as_str().to_owned(),
		VariableField::Array(items, _) => "todo".to_owned(),
		VariableField::Object(_, _) => "todo".to_owned(),
	}
}

fn synthesize_parameter_type_reference<T: crate::FSResolver>(
	type_reference: &Option<TypeReference>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	if let Some(type_reference) = type_reference {
		// Notice here the **true** here means to do new_inferred_generic_type_parameter if it finds a any at any depth
		let parameter_constraint =
			environment.get_type_handle_errors(type_reference, checking_data);

		if !matches!(
			checking_data.types.get_type_by_id(parameter_constraint),
			Type::RootPolyType { .. }
		) {
			checking_data.types.new_type(Type::RootPolyType(PolyNature::Parameter {
				fixed_to: crate::types::PolyPointer::Fixed(parameter_constraint),
			}))
		} else {
			parameter_constraint
		}
	} else {
		checking_data.types.new_any_parameter(environment)
	}
}

/// This synthesizes is for function types, references and interfaces.
///
/// TODO should always take effect annotations (right?)
pub(crate) fn type_function_reference<T: crate::FSResolver, S: ContextType>(
	type_parameters: &Option<Vec<GenericTypeConstraint>>,
	parameters: &parser::type_references::TypeReferenceFunctionParameters,
	// This Option rather than Option because function type references are always some
	return_type: Option<&TypeReference>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	position: parser::Span,
	nature: FunctionNature,
) -> FunctionType {
	let (parameters, return_type, generic_type_parameters, effects) =
		if let Some(type_parameters) = type_parameters {
			environment
				.new_lexical_environment_fold_into_parent(
					Scope::FunctionReference {},
					checking_data,
					|environment, checking_data| {
						let type_parameters: GenericTypeParameters = type_generic_type_constraints(
							&type_parameters,
							environment,
							checking_data,
							None,
						)
						.into();

						let parameters = type_function_parameters_from_reference(
							parameters,
							environment,
							checking_data,
						);

						let return_type = if let Some(return_type) = return_type {
							environment.get_type_handle_errors(return_type, checking_data)
						} else {
							TypeId::UNDEFINED_TYPE
						};

						let effects = todo!();

						// let effects = if let Some(event_markers) = effect_annotations {
						// 	synthesize_effect_annotations(event_markers, environment, checking_data)
						// } else {
						// 	Vec::new()
						// };

						(parameters, return_type, Some(type_parameters), effects)
					},
				)
				.0
		} else {
			environment
				.new_lexical_environment_fold_into_parent(
					Scope::FunctionReference {},
					checking_data,
					|environment, checking_data| {
						let parameters = type_function_parameters_from_reference(
							parameters,
							environment,
							checking_data,
						);

						let return_type = if let Some(return_type) = return_type {
							environment.get_type_handle_errors(return_type, checking_data)
						} else {
							TypeId::UNDEFINED_TYPE
						};

						let effects = todo!();

						// let effects =
						// 	if let Some(event_markers) = todo!() { todo!() } else { Vec::new() };

						(parameters, return_type, None, effects)
					},
				)
				.0
		};

	let generic_type_parameters = if let Some(generic_type_parameters) = generic_type_parameters {
		GenericFunctionTypeParameters::TypedParameters(generic_type_parameters)
	} else {
		GenericFunctionTypeParameters::None
	};

	FunctionType {
		parameters,
		return_type,
		generic_type_parameters,
		effects,
		closed_over_references: Default::default(),
		nature,
	}
}

// TODO not sure about this trait
pub(crate) trait ToFunctionNature: FunctionBased {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature;
}

impl ToFunctionNature for parser::functions::bases::ArrowFunctionBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		FunctionNature::Arrow
	}
}

impl ToFunctionNature for parser::functions::bases::StatementFunctionBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		let function_prototype = TypeId::NULL_TYPE;
		crate::utils::notify!("Temp function without prototype constraint (needed on ExpressionOrStatement on parser)");
		FunctionNature::Function { function_prototype }
	}
}

impl ToFunctionNature for parser::functions::bases::ExpressionFunctionBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		let function_prototype = todo!();
		FunctionNature::Function { function_prototype }
	}
}

impl ToFunctionNature for parser::functions::bases::ClassConstructorBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		FunctionNature::ClassConstructor { class_prototype: todo!(), class_constructor: todo!() }
	}
}
impl ToFunctionNature for parser::functions::bases::ClassFunctionBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		FunctionNature::Arrow
	}
}
impl ToFunctionNature for parser::functions::bases::ObjectLiteralMethodBase {
	fn get_function_nature<T: crate::FSResolver>(
		this: &FunctionBase<Self>,
		checking_data: &mut CheckingData<T>,
	) -> FunctionNature {
		FunctionNature::Arrow
	}
}

/// Synthesizes the whole function, block and parameters
pub(crate) fn synthesize_function<T: crate::FSResolver, BasedOnWhat: ToFunctionNature>(
	function: &mut FunctionBase<BasedOnWhat>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> FunctionType
where
	BasedOnWhat::Body: SynthesizableFunctionBody,
{
	// TODO chain bad here, needs to get stored during discovery...
	let mut chain = Chain::new();
	let chain: &mut Annex<Chain> = &mut Annex::new(&mut chain);

	let function_id = function.get_function_id();

	let expected_return_type = function
		.return_type
		.as_ref()
		.map(|reference| environment.get_type_handle_errors(reference, checking_data));

	// For recursion
	// checking_data
	// 	.functions
	// 	.currently_checking_functions
	// 	.insert(universal_function_id, expected_return_type);

	let nature: FunctionNature = BasedOnWhat::get_function_nature(function, checking_data);

	// TODO mutually exclusive things being not under same condition here
	// let left = Either::Left(function.function_id);
	let (this_extends_something, constructor_on) =
		if let FunctionNature::ClassConstructor { class_prototype, class_constructor } = nature {
			let ty = checking_data.types.get_type_by_id(class_prototype);
			let class_extends = matches!(ty, Type::AliasTo { .. });

			(class_extends, Some(class_constructor))
		} else {
			// TODO super may come up in functions and extends methods (inc getters)
			(false, None)
		};

	let environment_type = Scope::Function {
		// TODO add that it can be fixed by a parameter named this at the start
		this_constraint: TypeId::ERROR_TYPE,
		constructor_on,
		this_extends: this_extends_something,
	};

	let ((parameters, return_type, generic_type_parameters), stuff, _) = environment
		.new_lexical_environment_fold_into_parent(
			environment_type,
			checking_data,
			|environment, checking_data| {
				// TODO not great:
				if !this_extends_something && constructor_on.is_some() {
					todo!("Via function nature trait")
					// let left = Either::Left(function.get_function_id());
					// crate::utils::notify!("left = {:?}", left);
					// let ConstructorInformation { fields, class_instance_ty: constructs, .. } =
					// 	checking_data.functions.constructor_information.get(&left).unwrap();

					// environment.create_this(*constructs);
					// expressions::synthesize_class_fields(
					// 	fields.clone(),
					// 	environment,
					// 	checking_data,
					// 	&mut Annex::new(&mut Chain::new()),
					// );
				}

				let type_parameters = if let Some(ref type_parameters) = function.type_parameters {
					let type_generic_type_constraints = type_generic_type_constraints(
						&type_parameters,
						environment,
						checking_data,
						None,
					);
					GenericFunctionTypeParameters::TypedParameters(type_generic_type_constraints)
				} else {
					GenericFunctionTypeParameters::None
				};

				let function_parameters = type_function_parameters(
					&mut function.parameters,
					environment,
					checking_data,
					chain,
				);

				// Synthesize body here:
				let return_type =
					function.body.synthesize_function_body(environment, checking_data, chain);

				// Constructors automatically return themselves
				let return_type = match nature {
					FunctionNature::Arrow => return_type,
					FunctionNature::ClassConstructor { .. } => {
						crate::utils::notify!(
							"TODO conditional based on return type being primitive"
						);
						if let CanUseThis::ConstructorCalled { this_ty } = environment.can_use_this
						{
							this_ty
						} else {
							unreachable!("Super not called :( {:?}", environment.can_use_this)
						}
					}
					FunctionNature::Function { function_prototype: function } => {
						crate::utils::notify!("TODO conditional based on new.target");
						return_type
					}
				};

				(function_parameters, return_type, type_parameters)
			},
		);

	// TODO might be higher up
	if let Some(ref reference) = function.return_type {
		let ty = environment.get_type_handle_errors(reference, checking_data);

		let mut basic_subtyping = BasicEquality {
			add_property_restrictions: false,
			position: reference.get_position().into_owned(),
		};
		let type_is_subtype = type_is_subtype(
			ty,
			return_type,
			None,
			&mut basic_subtyping,
			environment,
			&checking_data.types,
		);

		if let SubTypeResult::IsNotSubType(_) = type_is_subtype {
			checking_data.diagnostics_container.add_error(
				TypeCheckError::ReturnedTypeDoesNotMatch {
					expected_return_type: TypeStringRepresentation::from_type_id(
						ty,
						&environment.into_general_environment(),
						&checking_data.types,
						checking_data.settings.debug_types,
					),
					returned_type: TypeStringRepresentation::from_type_id(
						return_type,
						&environment.into_general_environment(),
						&checking_data.types,
						checking_data.settings.debug_types,
					),
					position: reference.get_position().into_owned(),
				},
			);
		}
	}

	let (events, closed_over_references) = stuff.unwrap();

	// crate::utils::notify!("Events on '{:?}' function {:?}", function.function_id, events);

	// debug_assert!(present.is_some());

	// if let Some(constraint) = environment.get_deferred_function_constraint(universal_function_id) {
	// 	todo!(
	// 		"check that the constraint made at sites is okay with the value this function returned"
	// 	)
	// }

	FunctionType {
		parameters,
		return_type,
		generic_type_parameters,
		effects: events,
		closed_over_references,
		nature,
	}
}

pub(crate) trait SynthesizableFunctionBody {
	// Return type is the return type of the body, if it doesn't use
	/// any returns it is equal to [Type::Undefined]
	fn synthesize_function_body<T: crate::FSResolver>(
		&mut self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		chain: &mut Annex<Chain>,
	) -> TypeId;
}

impl SynthesizableFunctionBody for Block {
	fn synthesize_function_body<T: crate::FSResolver>(
		&mut self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		chain: &mut Annex<Chain>,
	) -> TypeId {
		let result = synthesize_block(self, environment, checking_data, chain);
		crate::utils::notify!(
			"TODO return value finding using events doesn't look at conditionals"
		);
		environment
			.context_type
			.events
			.iter()
			.find_map(
				|event| if let Event::Return { returned } = event { Some(*returned) } else { None },
			)
			.unwrap_or(TypeId::UNDEFINED_TYPE)
	}
}

impl SynthesizableFunctionBody for ExpressionOrBlock {
	fn synthesize_function_body<T: crate::FSResolver>(
		&mut self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
		chain: &mut Annex<Chain>,
	) -> TypeId {
		match self {
			ExpressionOrBlock::Expression(expression) => {
				synthesize_expression(expression, environment, checking_data, chain)
			}
			ExpressionOrBlock::Block(block) => {
				block.synthesize_function_body(environment, checking_data, chain)
			}
		}
	}
}

#[cfg(test)]
mod tests {

	use crate::{context::Root, TypeMappings};

	fn _get_base_environment() -> (Root, TypeMappings) {
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
		//     &mut DiagnosticsContainer::default(),
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

	//     let function_type = synthesize_function(
	//         &mut function.base,
	//         &environment,
	//         &mut Default::default(),
	//         &type_mappings,
	//         None
	//     );

	//     todo!("{:#?}", function_type);
	// }
}

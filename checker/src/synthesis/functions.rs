//! Function tings. Contains parameter synthesis, function body synthesis

use std::mem;

use parser::{
	expressions::ExpressionOrBlock, ASTNode, Block, FunctionBase, FunctionBased,
	GenericTypeConstraint, TypeAnnotation, VariableField, VariableIdentifier, WithComment,
};
use source_map::Span;

use crate::{
	behavior::functions::{GetterSetterGeneratorOrNone, SynthesizableFunction},
	context::{CanUseThis, Context, ContextType, Scope},
	types::poly_types::GenericTypeParameters,
	types::{
		functions::{SynthesizedParameter, SynthesizedParameters, SynthesizedRestParameter},
		poly_types::generic_type_arguments::TypeArgumentStore,
		FunctionKind, FunctionType, StructureGenerics,
	},
	types::{Constructor, Type, TypeId},
	CheckingData, Environment, FunctionId,
};

use super::{
	expressions::synthesize_expression, hoisting::string_comment_to_type, synthesize_block,
	type_annotations::synthesize_type_annotation, variables::register_variable, Performs,
};

trait FunctionBasedItem: FunctionBased {
	type ObjectTypeId;

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone;

	fn is_async(func: &FunctionBase<Self>) -> bool;
}

impl FunctionBasedItem for parser::functions::bases::StatementFunctionBase {
	type ObjectTypeId = ();

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		if func.header.is_generator() {
			GetterSetterGeneratorOrNone::Generator
		} else {
			GetterSetterGeneratorOrNone::None
		}
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		func.header.is_async()
	}
}

impl FunctionBasedItem for parser::functions::bases::ExpressionFunctionBase {
	type ObjectTypeId = ();

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		if func.header.is_generator() {
			GetterSetterGeneratorOrNone::Generator
		} else {
			GetterSetterGeneratorOrNone::None
		}
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		func.header.is_async()
	}
}

impl FunctionBasedItem for parser::functions::bases::ArrowFunctionBase {
	type ObjectTypeId = ();

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		GetterSetterGeneratorOrNone::None
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		func.header.is_some()
	}
}

impl<'a> From<&'a parser::GetSetGeneratorOrNone> for crate::GetterSetterGeneratorOrNone {
	fn from(value: &'a parser::GetSetGeneratorOrNone) -> Self {
		match value {
			parser::GetSetGeneratorOrNone::Get(_) => GetterSetterGeneratorOrNone::Getter,
			parser::GetSetGeneratorOrNone::Set(_) => GetterSetterGeneratorOrNone::Setter,
			parser::GetSetGeneratorOrNone::Generator(_)
			| parser::GetSetGeneratorOrNone::GeneratorStar(_) => GetterSetterGeneratorOrNone::Generator,
			parser::GetSetGeneratorOrNone::None => GetterSetterGeneratorOrNone::None,
		}
	}
}

impl FunctionBasedItem for parser::functions::bases::ObjectLiteralMethodBase {
	type ObjectTypeId = Option<TypeId>;

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		From::from(&func.header.1)
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		func.header.0.is_some()
	}
}

impl FunctionBasedItem for parser::functions::bases::ClassFunctionBase {
	type ObjectTypeId = Option<TypeId>;

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		From::from(&func.header.1)
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		func.header.0.is_some()
	}
}

impl FunctionBasedItem for parser::functions::bases::ClassConstructorBase {
	type ObjectTypeId = Option<TypeId>;

	fn get_set_generator_or_none(func: &FunctionBase<Self>) -> GetterSetterGeneratorOrNone {
		GetterSetterGeneratorOrNone::None
	}

	fn is_async(func: &FunctionBase<Self>) -> bool {
		false
	}
}

impl<U: FunctionBased + 'static> SynthesizableFunction for parser::FunctionBase<U>
where
	U: FunctionBasedItem,
	U::Body: SynthesizableFunctionBody,
{
	fn is_declare(&self) -> bool {
		false
	}

	fn type_parameters<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<GenericTypeParameters> {
		self.type_parameters
			.as_ref()
			.map(|ty_params| synthesize_type_parameters(&ty_params, environment, checking_data))
	}

	fn id(&self) -> FunctionId {
		let pos = self.get_position().into_owned();
		FunctionId(pos.source, pos.start)
	}

	fn this_constraint<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<TypeId> {
		// TODO
		None
	}

	fn parameters<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> SynthesizedParameters {
		synthesize_function_parameters(&self.parameters, environment, checking_data)
	}

	fn body<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) {
		self.body.synthesize_function_body(environment, checking_data)
	}

	fn return_type_annotation<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<(TypeId, Span)> {
		self.return_type.as_ref().map(|reference| {
			(
				synthesize_type_annotation(reference, environment, checking_data),
				reference.get_position().into_owned(),
			)
		})
	}

	fn is_async(&self) -> bool {
		U::is_async(self)
	}

	fn get_set_generator_or_none(&self) -> GetterSetterGeneratorOrNone {
		U::get_set_generator_or_none(self)
	}
}

pub(super) trait SynthesizableFunctionBody {
	// Return type is the return type of the body, if it doesn't use
	/// any returns it is equal to [Type::Undefined]
	fn synthesize_function_body<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	);
}

impl SynthesizableFunctionBody for Block {
	fn synthesize_function_body<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) {
		synthesize_block(&self.0, environment, checking_data);
	}
}

impl SynthesizableFunctionBody for ExpressionOrBlock {
	fn synthesize_function_body<T: crate::FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) {
		match self {
			ExpressionOrBlock::Expression(expression) => {
				let returned = synthesize_expression(expression, environment, checking_data);
				environment.return_value(returned);
			}
			ExpressionOrBlock::Block(block) => {
				block.synthesize_function_body(environment, checking_data)
			}
		}
	}
}

pub(crate) fn synthesize_type_parameters<T: crate::FSResolver>(
	type_parameters: &[GenericTypeConstraint],
	environment: &mut crate::Environment,
	checking_data: &mut crate::CheckingData<T>,
) -> GenericTypeParameters {
	type_parameters
		.iter()
		.map(|constraint| match constraint {
			GenericTypeConstraint::Parameter { name, default } => {
				let default_type = default
					.as_ref()
					.map(|ta| synthesize_type_annotation(ta, environment, checking_data));
				environment.new_explicit_type_parameter(
					name.as_str(),
					None,
					default_type,
					&mut checking_data.types,
				)
			}
			GenericTypeConstraint::Extends(name, extends) => {
				let extends = synthesize_type_annotation(extends, environment, checking_data);
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
pub(super) fn type_function_parameters_from_reference<T: crate::FSResolver>(
	reference_parameters: &parser::type_annotations::TypeAnnotationFunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> SynthesizedParameters {
	let parameters = reference_parameters
		.parameters
		.iter()
		.enumerate()
		.map(|(idx, parameter)| {
			let parameter_type =
				synthesize_type_annotation(&parameter.type_annotation, environment, checking_data);

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
				.map(get_parameter_name)
				.unwrap_or_else(|| format!("parameter{}", idx));

			let missing_value =
				if parameter.is_optional { Some(TypeId::UNDEFINED_TYPE) } else { None };

			SynthesizedParameter {
				ty: parameter_type,
				name,
				position: parameter.get_position().into_owned(),
				missing_value,
			}
		})
		.collect();

	let rest_parameter = reference_parameters.rest_parameter.as_ref().map(|parameter| {
		let ty = synthesize_type_annotation(&parameter.type_annotation, environment, checking_data);
		let item_type = if let TypeId::ERROR_TYPE = ty {
			TypeId::ERROR_TYPE
		} else if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments,
		})) = checking_data.types.get_type_by_id(ty)
		{
			arguments.get_argument(TypeId::T_TYPE).unwrap()
		} else {
			todo!();
			// checking_data.diagnostics_container.add_error(
			// 	TypeCheckError::RestParameterAnnotationShouldBeArrayType(rest_parameter.get),
			// );
			TypeId::ERROR_TYPE
		};
		SynthesizedRestParameter {
			item_type,
			name: parameter.name.clone(),
			position: parameter.type_annotation.get_position().into_owned(),
		}
	});

	SynthesizedParameters { parameters, rest_parameter }
}

fn synthesize_function_parameters<T: crate::FSResolver>(
	ast_parameters: &parser::FunctionParameters,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) -> SynthesizedParameters {
	let parameters: Vec<_> = ast_parameters
		.parameters
		.iter()
		.map(|parameter| {
			let annotation = parameter
				.type_annotation
				.as_ref()
				.map(|reference| synthesize_type_annotation(reference, environment, checking_data))
				.or_else(|| {
					if let WithComment::PostfixComment(item, possible_declaration, position) =
						&parameter.name
					{
						string_comment_to_type(
							possible_declaration,
							position,
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

			SynthesizedParameter {
				name,
				ty: param_type,
				position: parameter.get_position().into_owned(),
				missing_value,
			}
		})
		.collect();

	for parameter in ast_parameters.rest_parameter.iter() {
		todo!()
		// super::variables::hoist_variable_identifier(&parameter.name, environment, is_constant);
	}
	SynthesizedParameters { parameters, rest_parameter: Default::default() }
}

fn param_name_to_string(param: &VariableField<parser::VariableFieldInSourceCode>) -> String {
	match param {
		VariableField::Name(name) => {
			if let VariableIdentifier::Standard(name, ..) = name {
				name.clone()
			} else {
				"".to_owned()
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

/// This synthesizes is for function types, references and interfaces.
///
/// TODO should always take effect annotations (right?)
pub(super) fn type_function_reference<T: crate::FSResolver, S: ContextType>(
	type_parameters: &Option<Vec<GenericTypeConstraint>>,
	parameters: &parser::type_annotations::TypeAnnotationFunctionParameters,
	// This Option rather than Option because function type references are always some
	return_type: Option<&TypeAnnotation>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	performs: super::Performs,
	position: parser::Span,
	kind: FunctionKind,
	on_interface: Option<TypeId>,
) -> FunctionType {
	environment
		.new_lexical_environment_fold_into_parent(
			Scope::FunctionReference {},
			checking_data,
			|environment, checking_data| {
				let type_parameters: Option<GenericTypeParameters> = if let Some(type_parameters) =
					type_parameters
				{
					Some(synthesize_type_parameters(type_parameters, environment, checking_data))
				} else {
					None
				};

				let parameters =
					type_function_parameters_from_reference(parameters, environment, checking_data);

				let return_type = return_type
					.as_ref()
					.map(|reference| {
						synthesize_type_annotation(reference, environment, checking_data)
					})
					.unwrap_or(TypeId::UNDEFINED_TYPE);

				let (effects, constant_id) = match performs {
					Performs::Block(block) => {
						// TODO new environment ?
						environment.can_use_this =
							CanUseThis::Yeah { this_ty: on_interface.unwrap() };
						synthesize_block(&block.0, environment, checking_data);
						(mem::take(&mut environment.facts.events), None)
					}
					Performs::Const(id) => (Default::default(), Some(id)),
					Performs::None => (Default::default(), Default::default()),
				};

				FunctionType {
					// TODO
					id: FunctionId(position.source, position.start),
					parameters,
					return_type,
					type_parameters,
					effects,
					used_parent_references: Default::default(),
					closed_over_variables: Default::default(),
					kind,
					constant_id,
				}
			},
		)
		.0
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

	//     let function_type = synthesize_function(
	//         &function.base,
	//         &environment,
	//         &Default::default(),
	//         &type_mappings,
	//         None
	//     );

	//     todo!("{:#?}", function_type);
	// }
}

use std::iter;

use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	Decorated, Expression, GenericTypeConstraint, TypeAnnotation,
};

use crate::{
	context::{
		Environment, {Context, ContextType},
	},
	synthesis::{property_key_as_type, type_annotations::synthesise_type_annotation},
	types::poly_types::GenericTypeParameters,
	CheckingData, Property, Type, TypeId,
};

/// Doesn't have any metadata yet
///
/// Returns the constructor
pub(super) fn synthesise_class_declaration<
	T: crate::FSResolver,
	S: ContextType,
	P: parser::ExpressionOrStatementPosition,
>(
	class: &Decorated<ClassDeclaration<P>>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
) -> TypeId {
	let Decorated { on: class, decorators, position } = class;

	// TODO type needs to be hoisted
	let parameters =
		if let Some(ref type_parameters) = class.type_parameters { todo!() } else { None };

	// TODO what about no name
	let name = P::as_option_str(&class.name).unwrap().to_owned();

	// TODO
	let ty = Type::NamedRooted { name, parameters };
	let class_type = checking_data.types.register_type(ty);
	if let Some(ref extends) = class.extends {
		todo!();
		// let extends = environment.get_type_handle_errors(extends, checking_data);
	};

	// TODO static
	// checking_data.type_mappings.types_to_types.insert(class.type_id, class_type);
	// let class_type = *checking_data.type_mappings.types_to_types.get(&class.type_id).unwrap();

	let (constructor, ..) = environment.new_lexical_environment_fold_into_parent(
		crate::context::Scope::ClassEnvironment {},
		checking_data,
		|environment, checking_data| {
			if let Some(parameters) =
				checking_data.types.get_type_by_id(class_type).get_parameters()
			{
				let generic_parameters = type_generic_type_constraints(
					class.type_parameters.as_ref().unwrap(),
					environment,
					checking_data,
					Some(parameters),
				);
			};

			let extends = if let Some(ref extends) = class.extends {
				fn build_extends_type<'a, T: crate::FSResolver>(
					mut extends: impl Iterator<Item = &'a TypeAnnotation>,
					environment: &mut Environment,
					checking_data: &mut CheckingData<T>,
					on: TypeId,
				) -> TypeId {
					let mut ty = synthesise_type_annotation(
						extends.next().unwrap(),
						environment,
						checking_data,
					);

					for reference in extends {
						let rhs = synthesise_type_annotation(reference, environment, checking_data);
						// TODOsynthesise_type_annotation
						ty = checking_data.types.register_type(Type::And(ty, rhs));
					}

					environment.bases.connect_extends(on, ty);

					ty
				}

				let result =
					build_extends_type(iter::once(extends), environment, checking_data, class_type);

				Some(result)
			} else {
				None
			};

			let mut class_constructor = None;
			let mut properties = Vec::<(TypeId, Expression)>::new();
			let mut static_properties = Vec::<(TypeId, Property)>::new();

			for member in class.members.iter() {
				match &member.on {
					ClassMember::Constructor(constructor) => {
						let ty = environment.new_function(
							checking_data,
							constructor,
							crate::RegisterAsType,
						);

						class_constructor = Some(ty);
					}
					ClassMember::Method(static_kw, function) => {
						let property_key = function.name.get_ast_ref();
						let private =
							matches!(property_key, parser::PropertyKey::Ident(_, _, true));
						let key = property_key_as_type(
							property_key,
							environment,
							&mut checking_data.types,
						);
						let property = environment.new_function(
							checking_data,
							function,
							crate::RegisterOnExistingObject,
						);

						if static_kw.is_some() {
							static_properties.push((key, property));
						} else {
							environment.facts.register_property(class_type, key, property, true);
							// TODO check not already exists

							// if let Some(existing_property) = existing_property {
							// 	// panic!("{:?} declared twice", key_ty);
							// }
						}
					}
					ClassMember::StaticBlock(..) => {}
					ClassMember::Property(_, _) => todo!(),
				}

				// match member {
				// 	ClassMember::Property(
				// 		is_static,
				// 		ClassProperty { key, type_annotation, value },
				// 	) => {
				// 		// Important, key is also evaluated **outside** of the constructor function
				// 		let key = property_key_as_type(key.get_ast(), environment);
				// 		if is_static.is_some() {
				// 			let value = if let Some(value) = value {
				// 				synthesise_expression(value, environment, checking_data)
				// 			} else {
				// 				TypeId::UNDEFINED_TYPE
				// 			};
				// 			static_properties.push((key, value));
				// 		} else {
				// 			if let Some(value) = value {
				// 				// TODO BAD CLONE
				// 				properties.push((key, Expression::clone(value)));
				// 			} else {
				// 				todo!("property with no value, what is the useful for, insert undefined")
				// 			}
				// 		}
				// 	}
				// 	ClassMember::Constructor(ext) => {
				// 	}
				// 	ClassMember::Function(is_static, ext) => {
				// 		todo!()
				// 		// if let Some(method) = GetFunction::<
				// 		// 	parser::statements::classes::ClassFunctionBase,
				// 		// >::get_function_ref(
				// 		// 	&checking_data.functions.functions, ext.0
				// 		// ) {
				// 		//
				// 		// } else {
				// 		// 	panic!()
				// 		// }
				// 	}
				// }
			}

			let class_constructor = if let Some(class_constructor) = class_constructor {
				class_constructor
			} else {
				todo!()
			};

			// // ORDER INCREDIBLY IMPORTANT HERE
			// environment.class_constructors.insert(class_type, constructor_type);

			// crate::utils::notify!(
			// 	"Added two way class binding, {:?} {:?}",
			// 	class_type,
			// 	constructor_type
			// );

			// TODO add static propertoes
			// for (static_key, static_value) in static_properties {
			// 	// TODO abstract, not here
			// 	environment.proofs.new_property(constructor_type, static_key, static_value, true);
			// 	environment.context_type.events.push(Event::Setter {
			// 		on: constructor_type,
			// 		new: static_key,
			// 		under: static_value,
			// 		reflects_dependency: None,
			// 		initialization: true,
			// 	});
			// }

			class_constructor
		},
	);

	constructor
}

pub(super) fn type_generic_type_constraints<T: crate::FSResolver>(
	unwrap: &[GenericTypeConstraint],
	environment: &mut Context<crate::context::Syntax>,
	checking_data: &mut CheckingData<T>,
	parameters: Option<Vec<TypeId>>,
) -> GenericTypeParameters {
	todo!()
}

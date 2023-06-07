use std::iter;

use parser::{
	declarations::{classes::ClassMember, ClassDeclaration},
	Chain, Decorated, Expression, FunctionId, TypeReference,
};
use temporary_annex::Annex;

use crate::{
	context::{
		Environment, {Context, ContextType},
	},
	types::poly_types::type_generic_type_constraints,
	types::Type,
	CheckingData, TypeId,
};

/// Doesn't have any metadata yet
///
/// Returns the constructor
pub(crate) fn synthesize_class_declaration<
	T: crate::FSResolver,
	S: ContextType,
	P: parser::ExpressionOrStatementPosition,
>(
	class: &mut Decorated<ClassDeclaration<P>>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	let Decorated { on: class, decorators } = class;

	// TODO type needs to be hoisted
	let parameters =
		if let Some(ref type_parameters) = class.type_parameters { todo!() } else { None };

	// TODO what about no name
	let name = P::as_option_str(&class.name).unwrap().to_owned();

	let ty = Type::NamedRooted { name, parameters };
	let class_type = checking_data.types.new_type(ty);
	if let Some(ref extends) = class.extends {
		todo!();
		// let extends = environment.get_type_handle_errors(extends, checking_data);
	};
	// TODO static
	checking_data.type_mappings.types_to_types.insert(class.type_id, class_type);
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
					mut extends: impl Iterator<Item = &'a TypeReference>,
					environment: &mut Environment,
					checking_data: &mut CheckingData<T>,
					on: TypeId,
				) -> TypeId {
					let mut ty =
						environment.get_type_handle_errors(extends.next().unwrap(), checking_data);

					for reference in extends {
						let rhs = environment.get_type_handle_errors(reference, checking_data);
						ty = checking_data.types.new_type(Type::And(ty, rhs));
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

			let mut class_constructor_function_id = None::<FunctionId>;
			let mut properties = Vec::<(TypeId, Expression)>::new();
			let mut static_properties = Vec::<(TypeId, TypeId)>::new();

			let functions = class.members.iter_mut().filter_map(|member| {
				if let ClassMember::Function(r#static, function) = &member.on {
					Some((r#static, function, &member.decorators))
				} else {
					None
				}
			});
			for (r#static, function, decorators) in functions {
				todo!()
				// let Decorated { on: member, decorators } = member;
				// match member {
				// 	ClassMember::Property(
				// 		is_static,
				// 		ClassProperty { key, type_reference, value },
				// 	) => {
				// 		// Important, key is also evaluated **outside** of the constructor function
				// 		let key = property_key_as_type(key.get_ast(), environment);
				// 		if is_static.is_some() {
				// 			let value = if let Some(value) = value {
				// 				synthesize_expression(value, environment, checking_data, chain)
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
				// 		// 	let key_ty = property_key_as_type(method.name.get_ast(), environment);
				// 		// 	let function_type = environment.new_function_type(
				// 		// 		FunctionPointer::Function(parser::FunctionId::ClassFunctionBase(
				// 		// 			ext.0,
				// 		// 		)),
				// 		// 		&method.header.1,
				// 		// 	);
				// 		// 	if is_static.is_some() {
				// 		// 		static_properties.push((key_ty, function_type));
				// 		// 	} else {
				// 		// 		let existing_property = crate::utils::add_property(
				// 		// 			environment,
				// 		// 			class_type,
				// 		// 			key_ty,
				// 		// 			function_type,
				// 		// 		);
				// 		// 		if let Some(existing_property) = existing_property {
				// 		// 			panic!("{:?} declared twice", key_ty);
				// 		// 		}
				// 		// 	}
				// 		// } else {
				// 		// 	panic!()
				// 		// }
				// 	}
				// }
			}

			// let (constructor_type, constructor_id) = match class_constructor_function_id {
			// 	Some(function_id) => {
			// 		let ty = environment.new_function_type(
			// 			FunctionPointer::Function(function_id.into()),
			// 			&Default::default(),
			// 		);

			// 		(ty, either::Left(function_id))
			// 	}
			// 	None => {
			// 		let auto_constructor_id = AutoConstructorId::new();
			// 		let ty = environment.new_function_type(
			// 			FunctionPointer::AutoConstructor(auto_constructor_id),
			// 			&Default::default(),
			// 		);

			// 		crate::utils::notify!("{:?} {:?}", environment.subtyping_constant_proofs, ty);

			// 		(ty, either::Right(auto_constructor_id))
			// 	}
			// };

			// // ORDER INCREDIBLY IMPORTANT HERE
			// environment.class_constructors.insert(class_type, constructor_type);

			// crate::utils::notify!(
			// 	"Added two way class binding, {:?} {:?}",
			// 	class_type,
			// 	constructor_type
			// );

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

			// checking_data.functions.constructor_information.insert(
			// 	constructor_id,
			// 	ConstructorInformation {
			// 		fields: properties,
			// 		class_instance_ty: class_type,
			// 		class_constructor_ty: constructor_type,
			// 	},
			// );
			// constructor_type
			todo!()
		},
	);

	constructor
}

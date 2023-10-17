//! TODO need to call interface things rather going with own implementation to generate actual DOM operations

use parser::{ASTNode, Expression, JSXAttribute, JSXElement, JSXNode, JSXRoot};

use crate::{
	behavior::objects::ObjectBuilder,
	call_type_handle_errors,
	context::Logical,
	diagnostics::{TypeCheckError, TypeStringRepresentation},
	synthesis::expressions::synthesise_expression,
	types::{
		subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
		SynthesisedArgument,
	},
	CheckingData, Constant, Environment, TypeId,
};

pub(crate) fn synthesise_jsx_root<T: crate::ReadFromFS>(
	jsx_root: &JSXRoot,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> TypeId {
	match jsx_root {
		JSXRoot::Element(element) => synthesise_jsx_element(element, environment, checking_data),
		JSXRoot::Fragment(_) => todo!(),
	}
}

pub(crate) fn synthesise_jsx_element<T: crate::ReadFromFS>(
	element: &JSXElement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> TypeId {
	let tag_name = element.tag_name.as_str();

	let tag_name_as_cst_ty =
		checking_data.types.new_constant_type(Constant::String(element.tag_name.clone()));

	let mut attributes_object =
		ObjectBuilder::new(None, &mut checking_data.types, &mut environment.facts);

	for attribute in element.attributes.iter() {
		let (name, attribute_value) = synthesise_attribute(attribute, environment, checking_data);
		attributes_object.append(
			environment,
			name,
			crate::Property::Value(attribute_value),
			crate::context::facts::PublicityKind::Public,
		);

		// let constraint = environment
		// 	.get_property_unbound(element_type, name, &checking_data.types)
		// 	.map(Logical::prop_to_type);
		// 	match constraint {
		// 		Some(attr_restriction) => {
		// 			let mut basic_subtyping = BasicEquality {
		// 				add_property_restrictions: false,
		// 				position: attribute
		// 					.get_position()
		// 					.clone()
		// 					.with_source(environment.get_source()),
		// 			};
		// 			let check = type_is_subtype(
		// 				attr_restriction,
		// 				attr_value,
		// 				None,
		// 				&mut basic_subtyping,
		// 				environment,
		// 				&checking_data.types,
		// 			);
		// 			match check {
		// 				SubTypeResult::IsSubType => {
		// 					new_element_object.append(
		// 						environment,
		// 						name,
		// 						crate::types::properties::Property::Value(attr_value),
		// 					);
		// 				}
		// 				SubTypeResult::IsNotSubType(_) => {
		// 					checking_data.diagnostics_container.add_error(
		// 						TypeCheckError::InvalidJSXAttribute {
		// 							attribute_name: match attribute {
		// 								JSXAttribute::Static(name, _, _)
		// 								| JSXAttribute::Dynamic(name, _, _)
		// 								| JSXAttribute::BooleanAttribute(name, _) => name.clone(),
		// 								JSXAttribute::Spread(_, _) => todo!(),
		// 								JSXAttribute::Shorthand(_) => todo!(),
		// 							},
		// 							attribute_type: TypeStringRepresentation::from_type_id(
		// 								attr_restriction,
		// 								&environment.as_general_context(),
		// 								&checking_data.types,
		// 								checking_data.settings.debug_types,
		// 							),
		// 							value_type: TypeStringRepresentation::from_type_id(
		// 								attr_value,
		// 								&environment.as_general_context(),
		// 								&checking_data.types,
		// 								checking_data.settings.debug_types,
		// 							),
		// 							attribute_type_site: (),
		// 							value_site: parser::ASTNode::get_position(attribute)
		// 								.clone()
		// 								.with_source(environment.get_source()),
		// 						},
		// 					);
		// 				}
		// 			}
		// 		}
		// 		None => {
		// 			todo!("Property does not exist on element");
		// 		}
		// 	}
	}

	// 	new_element_object.append(
	// 		environment,
	// 		name,
	// 		crate::types::properties::Property::Value(attr_value),
	// 	);
	// }

	let child_nodes = if let parser::JSXElementChildren::Children(ref children) = element.children {
		let mut synthesised_child_nodes = ObjectBuilder::new(
			Some(TypeId::ARRAY_TYPE),
			&mut checking_data.types,
			&mut environment.facts,
		);

		let children_iterator =
			children.iter().filter(|p| !matches!(p, JSXNode::LineBreak)).enumerate();

		for (idx, child) in children_iterator {
			// TODO idx bad! and should override item
			let property = checking_data
				.types
				.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

			let child = synthesise_jsx_child(child, environment, checking_data);
			synthesised_child_nodes.append(
				environment,
				property,
				crate::Property::Value(child),
				crate::context::facts::PublicityKind::Public,
			);
		}

		Some(synthesised_child_nodes.build_object())
	} else {
		None
	};

	// TODO cache or something?
	// TODO temp, to be worked out
	const JSX_NAME: &str = "JSXH";
	let position = element.get_position().clone().with_source(environment.get_source());
	let jsx_function =
		match environment.get_variable_or_error(JSX_NAME, position.clone(), checking_data) {
			Ok(ty) => ty.1,
			Err(_) => {
				todo!()
			}
		};

	let tag_name_argument = SynthesisedArgument::NonSpread {
		ty: tag_name_as_cst_ty,
		// TODO use tag name position
		position: position.clone(),
	};
	let attributes_argument = SynthesisedArgument::NonSpread {
		ty: attributes_object.build_object(),
		// TODO use arguments position
		position: position.clone(),
	};

	let mut args = vec![tag_name_argument, attributes_argument];
	if let Some(child_nodes) = child_nodes {
		// TODO position here
		args.push(SynthesisedArgument::NonSpread { ty: child_nodes, position: position.clone() })
	}

	call_type_handle_errors(
		jsx_function,
		crate::types::calling::CalledWithNew::None,
		crate::behavior::functions::ThisValue::UseParent,
		None,
		args,
		position.clone(),
		environment,
		checking_data,
	)
	.0

	// else {
	// 	match environment.get_variable_or_error(tag_name, todo!(), checking_data) {
	// 		Ok(element_variable) => {
	// 			let variable_type = element_variable.1.clone();

	// 			todo!();

	// 			// if let Some(Constant::FunctionReference(function)) =
	// 			// 	checking_data.types.get_constant_type(variable_type)
	// 			// {
	// 			// 	match function {
	// 			// 		FunctionPointer::Function(function_id) => {
	// 			// 			todo!()
	// 			// 			// checking_data
	// 			// 			// 	.synthesise_hoisted_function(
	// 			// 			// 		variable_type,
	// 			// 			// 		*function_id,
	// 			// 			// 		environment,
	// 			// 			// 	)
	// 			// 			// 	.expect("TODO recursive components");
	// 			// 		}
	// 			// 		FunctionPointer::Internal(..) | FunctionPointer::AutoConstructor(..) => {
	// 			// 			unreachable!()
	// 			// 		}
	// 			// 	}
	// 			// } else {
	// 			// 	todo!()
	// 			// };

	// 			// let function = environment.get_function(variable_type).unwrap().clone();

	// 			// let props_parameter = function.parameters.get_type_constraint_at_index(0).unwrap();

	// 			// let mut expected_props = environment.get_properties_on_type(props_parameter);

	// 			// let mut found_error = false;

	// 			// let mut synthesised_attributes = ObjectBuilder::new(None, environment);
	// 			// let mut synthesised_child_nodes =
	// 			// 	ObjectBuilder::new(Some(TypeId::NODE_LIST_TYPE), environment);

	// 			// for attribute in element.attributes.iter() {
	// 			// 	let (name, attr_value) =
	// 			// 		synthesise_attribute(attribute, environment, checking_data);

	// 			// 	let expected = expected_props.contains(&name);
	// 			// 	if expected {
	// 			// 		let attr_restriction = environment
	// 			// 			.get_property_constraint(props_parameter, name, &checking_data.types)
	// 			// 			.unwrap();

	// 			// 		let mut basic_subtyping = BasicEquality {
	// 			// 			add_property_restrictions: false,
	// 			// 			position: attribute.get_position().into_owned(),
	// 			// 		};
	// 			// 		let check = type_is_subtype(
	// 			// 			attr_restriction,
	// 			// 			attr_value,
	// 			// 			None,
	// 			// 			&mut basic_subtyping,
	// 			// 			environment,
	// 			// 			&checking_data.types,
	// 			// 		);
	// 			// 		match check {
	// 			// 			SubTypeResult::IsSubType => {
	// 			// 				synthesised_attributes.append(environment, name, attr_value);
	// 			// 			}
	// 			// 			SubTypeResult::IsNotSubType(_) => {
	// 			// 				found_error = true;
	// 			// 				checking_data.diagnostics_container.add_error(
	// 			// 					TypeCheckError::InvalidJSXAttribute {
	// 			// 						attribute_name: match attribute {
	// 			// 							JSXAttribute::Static(name, _, _)
	// 			// 							| JSXAttribute::Dynamic(name, _, _)
	// 			// 							| JSXAttribute::BooleanAttribute(name, _) => name.clone(),
	// 			// 							JSXAttribute::Spread(_, _) => todo!(),
	// 			// 							JSXAttribute::Shorthand(_) => todo!(),
	// 			// 						},
	// 			// 						attribute_type: TypeStringRepresentation::from_type_id(
	// 			// 							attr_restriction,
	// 			// 							&environment.into_general_context(),
	// 			// 							checking_data.settings.debug_types,
	// 			// 						),
	// 			// 						value_type: TypeStringRepresentation::from_type_id(
	// 			// 							attr_value,
	// 			// 							&environment.into_general_context(),
	// 			// 							checking_data.settings.debug_types,
	// 			// 						),
	// 			// 						attribute_type_site: (),
	// 			// 						value_site: parser::ASTNode::get_position(attribute)
	// 			// 							.into_owned(),
	// 			// 					},
	// 			// 				);
	// 			// 			}
	// 			// 		}
	// 			// 	} else {
	// 			// 		crate::utils::notify!("TODO additional property");
	// 			// 	}
	// 			// }

	// 			// if let JSXElementChildren::Children(ref mut children) = element.children {
	// 			// 	for (idx, child) in
	// 			// 		children.iter().filter(|p| !matches!(p, JSXNode::LineBreak)).enumerate()
	// 			// 	{
	// 			// 		let child = synthesise_jsx_child(child, environment, checking_data);

	// 			// 		// TODO idx bad
	// 			// 		// TODO should be done by item rather than idx
	// 			// 		let number = checking_data
	// 			// 			.types
	// 			// 			.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

	// 			// 		synthesised_child_nodes.append(environment, number, child);
	// 			// 	}
	// 			// }

	// 			// if expected_props.contains(&TypeId::CHILD_NODES_AS_STRING) {
	// 			// 	let synthesised_child_nodes = synthesised_child_nodes.build_object();
	// 			// 	synthesised_attributes.append(
	// 			// 		environment,
	// 			// 		TypeId::CHILD_NODES_AS_STRING,
	// 			// 		synthesised_child_nodes,
	// 			// 	);
	// 			// }

	// 			// if !found_error {
	// 			// 	let props = synthesised_attributes.build_object();

	// 			// 	function
	// 			// 		.call(
	// 			// 			&[synthesisedArgument::NonSpread {
	// 			// 				ty: props,
	// 			// 				position: Span { start: 0, end: 0, source_id: SourceId::NULL },
	// 			// 			}],
	// 			// 			None,
	// 			// 			None,
	// 			// 			&None,
	// 			// 			checking_data,
	// 			// 			environment,
	// 			// 			// TODO not sure, this won't work for classes right...
	// 			// 			crate::events::CalledWithNew::None,
	// 			// 		)
	// 			// 		.unwrap()
	// 			// 		.returned_type
	// 			// } else {
	// 			// 	TypeId::ERROR_TYPE
	// 			// }
	// 		}
	// 		Err(_) => {
	// 			// TODO deferred custom elements...?
	// 			crate::utils::notify!("add error");
	// 			// checking_data.add_error(TypeCheckErrors::from(err), element.get_position().into());
	// 			// return Ok(Instance::new_error_instance());
	// 			TypeId::ERROR_TYPE
	// 		}
	// 	}
}

fn synthesise_jsx_child<T: crate::ReadFromFS>(
	child: &JSXNode,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> TypeId {
	match child {
		JSXNode::Element(element) => synthesise_jsx_element(element, environment, checking_data),
		JSXNode::InterpolatedExpression(expression, expression_position) => {
			if matches!(&**expression, Expression::Comment(..)) {
				return TypeId::UNDEFINED_TYPE;
			}

			crate::utils::notify!("Cast to node!");
			synthesise_expression(expression, environment, checking_data)

			// function intoNode(data) {
			// 	if typeof data === "string" || typeof data === "number" {
			// 		new Text(data)
			// 	} else {
			// 		return data;
			// 	}
			// }

			// let expected_type = checking_data.memory.get_jsx_interpretable_values();
			// Checking expression is `string | number | HTMLElement | Array<string | number | HTMLElement>`
			// if let SubTypeResult::IsNotSubType(_mismatch) = type_is_subtype(
			//     &expected_type,
			//     &expression.as_type(),
			//     &mut GeneralEquality(&mut checking_data.memory),
			// ) {
			//     checking_data.diagnostics_container.add_error(
			//         TypeCheckError::InvalidJSXInterpolatedValue {
			//             interpolation_site: expression_position.clone().unwrap(),
			//             expected: TypeStringRepresentation::from_type(
			//                 &expected_type,
			//                 &checking_data.memory,
			//                 checking_data.settings.debug_types,
			//             ),
			//             found: TypeStringRepresentation::from_type(
			//                 &expression.as_type(),
			//                 &checking_data.memory,
			//                 checking_data.settings.debug_types,
			//             ),
			//         },
			//     );
			// }
		}
		JSXNode::TextNode(text, _) => {
			checking_data.types.new_constant_type(Constant::String(text.clone()))
		}
		JSXNode::LineBreak => {
			unreachable!("Should have been skipped higher up");
		}
	}
}

fn synthesise_attribute<T: crate::ReadFromFS>(
	attribute: &JSXAttribute,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> (TypeId, TypeId) {
	let (key, value) = match attribute {
		// TODO check property exists ...?
		JSXAttribute::Static(name, value, attribute_id) => {
			(name, checking_data.types.new_constant_type(crate::Constant::String(value.clone())))
		}
		JSXAttribute::Dynamic(name, expression, attribute_id) => {
			// Do not care about the returned value at this point, just for synthesizing the type into the map
			(name, synthesise_expression(expression, environment, checking_data))
		}
		JSXAttribute::BooleanAttribute(name, _) => (name, TypeId::TRUE),
		JSXAttribute::Spread(_, _) => todo!(),
		JSXAttribute::Shorthand(_) => todo!(),
	};

	(checking_data.types.new_constant_type(crate::Constant::String(key.clone())), value)
}

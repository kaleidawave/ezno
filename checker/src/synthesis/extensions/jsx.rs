//! TODO need to call interface things rather going with own implementation to generate actual DOM operations

use parser::{ASTNode, Chain, Expression, JSXAttribute, JSXElement, JSXNode, JSXRoot};
use temporary_annex::Annex;

use crate::{
	context::Logical,
	errors::{TypeCheckError, TypeStringRepresentation},
	structures::objects::ObjectBuilder,
	synthesis::synthesize_expression,
	types::subtyping::{type_is_subtype, BasicEquality, SubTypeResult},
	CheckingData, Constant, Environment, TypeId,
};

pub(crate) fn synthesize_jsx_root<T: crate::FSResolver>(
	jsx_root: &mut JSXRoot,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	match jsx_root {
		JSXRoot::Element(element) => {
			synthesize_jsx_element(element, environment, checking_data, chain)
		}
		JSXRoot::Fragment(_) => todo!(),
	}
}

pub(crate) fn synthesize_jsx_element<T: crate::FSResolver>(
	element: &mut JSXElement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	let tag_name = element.tag_name.as_str();

	let tag_name_as_cst_ty =
		checking_data.types.new_constant_type(Constant::String(element.tag_name.clone()));

	if let Some(element_type) = environment.get_tag_name(tag_name_as_cst_ty, &checking_data.types) {
		let mut new_element_object =
			ObjectBuilder::new(Some(element_type), &mut checking_data.types, environment);

		for attribute in element.attributes.iter_mut() {
			let (name, attr_value) =
				synthesize_attribute(attribute, environment, checking_data, chain);

			let constraint = environment
				.get_property_unbound(element_type, name, &checking_data.types)
				.map(Logical::to_type);

			match constraint {
				Some(attr_restriction) => {
					let mut basic_subtyping = BasicEquality {
						add_property_restrictions: false,
						position: attribute.get_position().into_owned(),
					};
					let check = type_is_subtype(
						attr_restriction,
						attr_value,
						None,
						&mut basic_subtyping,
						environment,
						&checking_data.types,
					);
					match check {
						SubTypeResult::IsSubtype => {
							new_element_object.append(environment, name, attr_value);
						}
						SubTypeResult::IsNotSubType(_) => {
							checking_data.diagnostics_container.add_error(
								TypeCheckError::InvalidJSXAttribute {
									attribute_name: match attribute {
										JSXAttribute::Static(name, _, _)
										| JSXAttribute::Dynamic(name, _, _)
										| JSXAttribute::BooleanAttribute(name, _) => name.clone(),
										JSXAttribute::Spread(_, _) => todo!(),
										JSXAttribute::Shorthand(_) => todo!(),
									},
									attribute_type: TypeStringRepresentation::from_type_id(
										attr_restriction,
										&environment.into_general_environment(),
										&checking_data.types,
										checking_data.settings.debug_types,
									),
									value_type: TypeStringRepresentation::from_type_id(
										attr_value,
										&environment.into_general_environment(),
										&checking_data.types,
										checking_data.settings.debug_types,
									),
									attribute_type_site: (),
									value_site: parser::ASTNode::get_position(attribute)
										.into_owned(),
								},
							);
						}
					}
				}
				None => {
					todo!("Property does not exist on element");
				}
			}

			new_element_object.append(environment, name, attr_value);
		}

		todo!();

		// let mut synthesized_child_nodes =
		// 	ObjectBuilder::new(Some(TypeId::NODE_LIST_TYPE), environment);

		// if let JSXElementChildren::Children(ref mut children) = element.children {
		// 	for (idx, child) in
		// 		children.iter_mut().filter(|p| !matches!(p, JSXNode::LineBreak)).enumerate()
		// 	{
		// 		// TODO idx bad! and should override item
		// 		let property = checking_data
		// 			.types
		// 			.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

		// 		let child = synthesize_jsx_child(child, environment, checking_data, chain);
		// 		synthesized_child_nodes.append(environment, property, child);
		// 	}

		// 	new_element_object.append(
		// 		environment,
		// 		TypeId::CHILD_NODES_AS_STRING,
		// 		synthesized_child_nodes.build_object(),
		// 	);
		// }
		new_element_object.build_object()
	} else {
		match environment.get_variable_or_alternatives(tag_name, &mut checking_data.types) {
			Ok(element_variable) => {
				let variable_type = element_variable.1.clone();

				todo!();

				// if let Some(Constant::FunctionReference(function)) =
				// 	checking_data.types.get_constant_type(variable_type)
				// {
				// 	match function {
				// 		FunctionPointer::Function(function_id) => {
				// 			todo!()
				// 			// checking_data
				// 			// 	.synthesize_hoisted_function(
				// 			// 		variable_type,
				// 			// 		*function_id,
				// 			// 		environment,
				// 			// 	)
				// 			// 	.expect("TODO recursive components");
				// 		}
				// 		FunctionPointer::Internal(..) | FunctionPointer::AutoConstructor(..) => {
				// 			unreachable!()
				// 		}
				// 	}
				// } else {
				// 	todo!()
				// };

				// let function = environment.get_function(variable_type).unwrap().clone();

				// let props_parameter = function.parameters.get_type_constraint_at_index(0).unwrap();

				// let mut expected_props = environment.get_properties_on_type(props_parameter);

				// let mut found_error = false;

				// let mut synthesized_attributes = ObjectBuilder::new(None, environment);
				// let mut synthesized_child_nodes =
				// 	ObjectBuilder::new(Some(TypeId::NODE_LIST_TYPE), environment);

				// for attribute in element.attributes.iter_mut() {
				// 	let (name, attr_value) =
				// 		synthesize_attribute(attribute, environment, checking_data, chain);

				// 	let expected = expected_props.contains(&name);
				// 	if expected {
				// 		let attr_restriction = environment
				// 			.get_property_constraint(props_parameter, name, &checking_data.types)
				// 			.unwrap();

				// 		let mut basic_subtyping = BasicEquality {
				// 			add_property_restrictions: false,
				// 			position: attribute.get_position().into_owned(),
				// 		};
				// 		let check = type_is_subtype(
				// 			attr_restriction,
				// 			attr_value,
				// 			None,
				// 			&mut basic_subtyping,
				// 			environment,
				// 			&checking_data.types,
				// 		);
				// 		match check {
				// 			SubTypeResult::IsSubtype => {
				// 				synthesized_attributes.append(environment, name, attr_value);
				// 			}
				// 			SubTypeResult::IsNotSubType(_) => {
				// 				found_error = true;
				// 				checking_data.diagnostics_container.add_error(
				// 					TypeCheckError::InvalidJSXAttribute {
				// 						attribute_name: match attribute {
				// 							JSXAttribute::Static(name, _, _)
				// 							| JSXAttribute::Dynamic(name, _, _)
				// 							| JSXAttribute::BooleanAttribute(name, _) => name.clone(),
				// 							JSXAttribute::Spread(_, _) => todo!(),
				// 							JSXAttribute::Shorthand(_) => todo!(),
				// 						},
				// 						attribute_type: TypeStringRepresentation::from_type_id(
				// 							attr_restriction,
				// 							&environment.into_general_environment(),
				// 							checking_data.settings.debug_types,
				// 						),
				// 						value_type: TypeStringRepresentation::from_type_id(
				// 							attr_value,
				// 							&environment.into_general_environment(),
				// 							checking_data.settings.debug_types,
				// 						),
				// 						attribute_type_site: (),
				// 						value_site: parser::ASTNode::get_position(attribute)
				// 							.into_owned(),
				// 					},
				// 				);
				// 			}
				// 		}
				// 	} else {
				// 		crate::utils::notify!("TODO additional property");
				// 	}
				// }

				// if let JSXElementChildren::Children(ref mut children) = element.children {
				// 	for (idx, child) in
				// 		children.iter_mut().filter(|p| !matches!(p, JSXNode::LineBreak)).enumerate()
				// 	{
				// 		let child = synthesize_jsx_child(child, environment, checking_data, chain);

				// 		// TODO idx bad
				// 		// TODO should be done by item rather than idx
				// 		let number = checking_data
				// 			.types
				// 			.new_constant_type(Constant::Number((idx as f64).try_into().unwrap()));

				// 		synthesized_child_nodes.append(environment, number, child);
				// 	}
				// }

				// if expected_props.contains(&TypeId::CHILD_NODES_AS_STRING) {
				// 	let synthesized_child_nodes = synthesized_child_nodes.build_object();
				// 	synthesized_attributes.append(
				// 		environment,
				// 		TypeId::CHILD_NODES_AS_STRING,
				// 		synthesized_child_nodes,
				// 	);
				// }

				// if !found_error {
				// 	let props = synthesized_attributes.build_object();

				// 	function
				// 		.call(
				// 			&[SynthesizedArgument::NonSpread {
				// 				ty: props,
				// 				position: Span { start: 0, end: 0, source_id: SourceId::NULL },
				// 			}],
				// 			None,
				// 			None,
				// 			&None,
				// 			checking_data,
				// 			environment,
				// 			// TODO not sure, this won't work for classes right...
				// 			crate::events::CalledWithNew::None,
				// 		)
				// 		.unwrap()
				// 		.returned_type
				// } else {
				// 	TypeId::ERROR_TYPE
				// }
			}
			Err(_) => {
				// TODO deferred custom elements...?
				crate::utils::notify!("add error");
				// checking_data.add_error(TypeCheckErrors::from(err), element.get_position().into());
				// return Ok(Instance::new_error_instance());
				TypeId::ERROR_TYPE
			}
		}
	}
}

fn synthesize_jsx_child<T: crate::FSResolver>(
	child: &mut JSXNode,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> TypeId {
	match child {
		JSXNode::Element(element) => {
			synthesize_jsx_element(element, environment, checking_data, chain)
		}
		JSXNode::InterpolatedExpression(expression, expression_position) => {
			if matches!(&**expression, Expression::Comment(..)) {
				return TypeId::UNDEFINED_TYPE;
			}

			crate::utils::notify!("Cast to node!");
			synthesize_expression(expression, environment, checking_data, chain)

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
			//     checking_data.error_warning_info_handler.add_error(
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
			todo!();
			// let mut object = ObjectBuilder::new(Some(TypeId::TEXT_TYPE), environment);

			// // TODO setting data on text when it exist on CharacterData, I think this is fine though
			// let content = checking_data.types.new_constant_type(Constant::String(text.clone()));
			// object.append(environment, TypeId::DATA_AS_STRING, content);

			// object.build_object()
		}
		JSXNode::LineBreak => {
			unreachable!("Should have been skipped higher up");
		}
	}
}

fn synthesize_attribute<T: crate::FSResolver>(
	attribute: &mut JSXAttribute,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) -> (TypeId, TypeId) {
	let (key, value) = match attribute {
		// TODO check property exists ...?
		JSXAttribute::Static(name, value, attribute_id) => {
			(name, checking_data.types.new_constant_type(crate::Constant::String(value.clone())))
		}
		JSXAttribute::Dynamic(name, expression, attribute_id) => {
			// Do not care about the returned value at this point, just for synthesizing the type into the map
			let attribute_instance =
				synthesize_expression(expression, environment, checking_data, chain);

			(name, attribute_instance)
		}
		JSXAttribute::BooleanAttribute(name, _) => (name, TypeId::TRUE),
		JSXAttribute::Spread(_, _) => todo!(),
		JSXAttribute::Shorthand(_) => todo!(),
	};

	(checking_data.types.new_constant_type(crate::Constant::String(key.clone())), value)
}

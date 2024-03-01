//! TODO need to call interface things rather going with own implementation to generate actual DOM operations

use std::borrow::Cow;

use parser::{ASTNode, Expression, JSXAttribute, JSXElement, JSXNode, JSXRoot};

use crate::{
	context::invocation::CheckThings,
	features::objects::ObjectBuilder,
	synthesis::expressions::synthesise_expression,
	types::{
		calling::{call_type, CallingInput},
		properties::PropertyKey,
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
	// TODO cache or something?
	// TODO temp, to be worked out
	const JSX_NAME: &str = "JSXH";

	let _tag_name = element.tag_name.as_str();

	let tag_name_as_cst_ty =
		checking_data.types.new_constant_type(Constant::String(element.tag_name.clone()));

	let mut attributes_object =
		ObjectBuilder::new(None, &mut checking_data.types, &mut environment.facts);

	for attribute in &element.attributes {
		let (name, attribute_value) = synthesise_attribute(attribute, environment, checking_data);
		let attribute_position = attribute.get_position().with_source(environment.get_source());
		attributes_object.append(
			environment,
			crate::context::facts::Publicity::Public,
			name,
			crate::PropertyValue::Value(attribute_value),
			Some(attribute_position),
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
		// 								checking_data.options.debug_types,
		// 							),
		// 							value_type: TypeStringRepresentation::from_type_id(
		// 								attr_value,
		// 								&environment.as_general_context(),
		// 								&checking_data.types,
		// 								checking_data.options.debug_types,
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

		let children_iterator = children
			.iter()
			.filter(|p| !matches!(p, JSXNode::LineBreak | JSXNode::Comment(..)))
			.enumerate();

		for (idx, child) in children_iterator {
			// TODO idx bad! and should override item
			let property = PropertyKey::from_usize(idx);

			let child_position = child.get_position().with_source(environment.get_source());
			let child = synthesise_jsx_child(child, environment, checking_data);
			synthesised_child_nodes.append(
				environment,
				crate::context::facts::Publicity::Public,
				property,
				crate::PropertyValue::Value(child),
				Some(child_position),
			);
		}

		Some(synthesised_child_nodes.build_object())
	} else {
		None
	};

	let position = element.get_position().with_source(environment.get_source());
	let jsx_function =
		match environment.get_variable_handle_error(JSX_NAME, position, checking_data) {
			Ok(ty) => ty.1,
			Err(_) => {
				todo!()
			}
		};

	let tag_name_argument = SynthesisedArgument {
		value: tag_name_as_cst_ty,
		spread: false,
		// TODO use tag name position
		position,
	};
	let attributes_argument = SynthesisedArgument {
		value: attributes_object.build_object(),
		spread: false,
		// TODO use arguments position
		position,
	};

	let mut args = vec![tag_name_argument, attributes_argument];
	if let Some(child_nodes) = child_nodes {
		// TODO position here
		args.push(SynthesisedArgument { value: child_nodes, position, spread: false });
	}

	match call_type(
		jsx_function,
		args,
		CallingInput {
			called_with_new: crate::types::calling::CalledWithNew::None,
			this_value: environment.facts.value_of_this,
			call_site: position,
			call_site_type_arguments: None,
		},
		environment,
		&mut CheckThings,
		&mut checking_data.types,
	) {
		Ok(res) => res.returned_type,
		Err(_) => {
			todo!("JSX Calling error")
		}
	}

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
	// 			// 							checking_data.options.debug_types,
	// 			// 						),
	// 			// 						value_type: TypeStringRepresentation::from_type_id(
	// 			// 							attr_value,
	// 			// 							&environment.into_general_context(),
	// 			// 							checking_data.options.debug_types,
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
	// 			// 				position: Span { start: 0, end: 0, source_id: source_map::Nullable::NULL },
	// 			// 			}],
	// 			// 			None,
	// 			// 			None,
	// 			// 			&None,
	// 			// 			checking_data,
	// 			// 			environment,
	// 			// 			// TODO unsure, this won't work for classes right...
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

/// TODO function argument
fn synthesise_jsx_child<T: crate::ReadFromFS>(
	child: &JSXNode,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> TypeId {
	match child {
		JSXNode::Element(element) => synthesise_jsx_element(element, environment, checking_data),
		JSXNode::InterpolatedExpression(expression, _expression_position) => {
			match &**expression {
				parser::ast::FunctionArgument::Spread(_, _) => todo!(),
				parser::ast::FunctionArgument::Standard(expression) => {
					crate::utils::notify!("Cast JSX interpolated value?");
					synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE)
				}
				parser::ast::FunctionArgument::Comment { .. } => {
					todo!()
				}
			}
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
			//                 checking_data.options.debug_types,
			//             ),
			//             found: TypeStringRepresentation::from_type(
			//                 &expression.as_type(),
			//                 &checking_data.memory,
			//                 checking_data.options.debug_types,
			//             ),
			//         },
			//     );
			// }
		}
		JSXNode::TextNode(text, _) => {
			checking_data.types.new_constant_type(Constant::String(text.clone()))
		}
		JSXNode::LineBreak | JSXNode::Comment(..) => {
			unreachable!("Should have been skipped higher up");
		}
	}
}

fn synthesise_attribute<T: crate::ReadFromFS>(
	attribute: &JSXAttribute,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, crate::synthesis::EznoParser>,
) -> (PropertyKey<'static>, TypeId) {
	let (key, value) = match attribute {
		// TODO check property exists ...?
		JSXAttribute::Static(name, value, _attribute_id) => {
			(name, checking_data.types.new_constant_type(crate::Constant::String(value.clone())))
		}
		JSXAttribute::Dynamic(name, expression, _attribute_id) => {
			if let Expression::ExpressionFunction(_) = &**expression {
				// TODO temp context
				environment.context_type.location = Some("client".to_owned());
			}
			// Do not care about the returned value at this point, just for synthesising the type into the map
			// TODO expecting
			(name, synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE))
		}
		JSXAttribute::BooleanAttribute(name, _) => (name, TypeId::TRUE),
		JSXAttribute::Spread(_, _) => todo!(),
		JSXAttribute::Shorthand(_) => todo!(),
	};

	(PropertyKey::String(Cow::Owned(key.clone())), value)
}

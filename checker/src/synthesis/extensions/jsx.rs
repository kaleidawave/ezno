//! TODO need to call interface things rather going with own implementation to generate actual DOM operations

use std::borrow::Cow;

use parser::{
	jsx::{JSXAttribute, JSXElement, JSXElementChildren, JSXNode, JSXRoot},
	ASTNode, Expression,
};

use crate::{
	context::invocation::CheckSyntax,
	diagnostics::TypeCheckError,
	features::objects::ObjectBuilder,
	synthesis::expressions::synthesise_expression,
	types::{
		calling::{
			application_result_to_return_type, Callable, CalledWithNew, CallingContext,
			CallingInput, SynthesisedArgument,
		},
		properties::PropertyKey,
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
		JSXRoot::Fragment(fragment) => {
			checking_data.raise_unimplemented_error(
				"JSX fragment",
				fragment.get_position().with_source(environment.get_source()),
			);
			TypeId::UNIMPLEMENTED_ERROR_TYPE
		}
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

	let mut attributes_object = ObjectBuilder::new(
		None,
		&mut checking_data.types,
		element.position.with_source(environment.get_source()),
		&mut environment.info,
	);

	for attribute in &element.attributes {
		let (name, attribute_value) = synthesise_attribute(attribute, environment, checking_data);
		let attribute_position = attribute.get_position().with_source(environment.get_source());
		attributes_object.append(
			crate::types::properties::Publicity::Public,
			name,
			crate::PropertyValue::Value(attribute_value),
			attribute_position,
			&mut environment.info,
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
		// 				todo!(),
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
		// 					checking_data.add_error(
		// 						TypeCheckError::InvalidJSXAttribute {
		// 							attribute_name: match attribute {
		// 								JSXAttribute::Static(name, _, _)
		// 								| JSXAttribute::Dynamic(name, _, _)
		// 								| JSXAttribute::Boolean(name, _) => name.clone(),
		// 								JSXAttribute::Spread(_, _) => todo!(),
		// 								JSXAttribute::Shorthand(_) => todo!(),
		// 							},
		// 							attribute_type: TypeStringRepresentation::from_type_id(
		// 								attr_restriction,
		// 								environment,
		// 								&checking_data.types,
		// 								checking_data.options.debug_types,
		// 							),
		// 							value_type: TypeStringRepresentation::from_type_id(
		// 								attr_value,
		// 								environment,
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

	let child_nodes = if let JSXElementChildren::Children(ref children) = element.children {
		// fn get_children() {
		let mut synthesised_child_nodes = ObjectBuilder::new(
			Some(TypeId::ARRAY_TYPE),
			&mut checking_data.types,
			element.position.with_source(environment.get_source()),
			&mut environment.info,
		);

		let children_iterator = children
			.iter()
			.filter(|p| !matches!(p, JSXNode::LineBreak | JSXNode::Comment(..)))
			.enumerate();

		let mut count = 0;
		for (idx, child) in children_iterator {
			// TODO idx bad! and should override item
			let property = PropertyKey::from_usize(idx);

			let child_position = child.get_position().with_source(environment.get_source());
			let child = synthesise_jsx_child(child, environment, checking_data);
			synthesised_child_nodes.append(
				crate::types::properties::Publicity::Public,
				property,
				crate::PropertyValue::Value(child),
				child_position,
				&mut environment.info,
			);

			// TODO spread ??
			count += 1;
		}

		{
			// TODO spread
			let length = checking_data.types.new_constant_type(Constant::Number(f64::from(count)));

			// TODO: Should there be a position here?
			synthesised_child_nodes.append(
				crate::types::properties::Publicity::Public,
				crate::types::properties::PropertyKey::String("length".into()),
				crate::types::properties::PropertyValue::Value(length),
				element.get_position().with_source(environment.get_source()),
				&mut environment.info,
			);
		}
		// }

		Some(synthesised_child_nodes.build_object())
	} else {
		None
	};

	let position = element.get_position().with_source(environment.get_source());
	let jsx_function =
		if let Ok(ty) = environment.get_variable_handle_error(JSX_NAME, position, checking_data) {
			ty.1
		} else {
			let error = TypeCheckError::CouldNotFindVariable {
				variable: JSX_NAME,
				possibles: Vec::default(),
				position,
			};
			checking_data.add_error(error, environment);
			TypeId::ERROR_TYPE
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

	let mut check_syntax = CheckSyntax { debug_types: checking_data.options.debug_types };

	let calling_input = CallingInput {
		called_with_new: CalledWithNew::None,
		call_site: position,
		max_inline: checking_data.options.max_inline_count,
	};

	let result = Callable::Type(jsx_function).call(
		args,
		calling_input,
		environment,
		(&mut check_syntax, &mut checking_data.resolver),
		&mut checking_data.types,
	);
	todo!();
	// diagnostics.append_to(CallingContext::JSX, &mut checking_data.diagnostics_container);

	match result {
		Ok(res) => {
			application_result_to_return_type(res.result, environment, &mut checking_data.types)
		}
		Err(error) => error.returned_type,
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
	// 			// 			todo!(),
	// 			// 			environment,
	// 			// 			&checking_data.types,
	// 			// 		);
	// 			// 		match check {
	// 			// 			SubTypeResult::IsSubType => {
	// 			// 				synthesised_attributes.append(environment, name, attr_value);
	// 			// 			}
	// 			// 			SubTypeResult::IsNotSubType(_) => {
	// 			// 				found_error = true;
	// 			// 				checking_data.add_error(
	// 			// 					TypeCheckError::InvalidJSXAttribute {
	// 			// 						attribute_name: match attribute {
	// 			// 							JSXAttribute::Static(name, _, _)
	// 			// 							| JSXAttribute::Dynamic(name, _, _)
	// 			// 							| JSXAttribute::Boolean(name, _) => name.clone(),
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
	// 			// 		crate::utilities::notify!("TODO additional property");
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
	// 			crate::utilities::notify!("add error");
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
				parser::ast::FunctionArgument::Spread(_, pos) => {
					checking_data.raise_unimplemented_error(
						"spread JSX child",
						pos.with_source(environment.get_source()),
					);
					TypeId::UNDEFINED_TYPE
				}
				parser::ast::FunctionArgument::Standard(expression) => {
					crate::utilities::notify!("Cast JSX interpolated value?");
					synthesise_expression(expression, environment, checking_data, TypeId::ANY_TYPE)
				}
				parser::ast::FunctionArgument::Comment { .. } => {
					// TODO?
					TypeId::UNDEFINED_TYPE
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
			//     checking_data.add_error(
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
		JSXAttribute::Boolean(name, _) => (name, TypeId::TRUE),
		JSXAttribute::Spread(_, pos) => {
			checking_data.raise_unimplemented_error(
				"spread JSX attribute",
				pos.with_source(environment.get_source()),
			);
			return (PropertyKey::String(Cow::Borrowed("err")), TypeId::UNIMPLEMENTED_ERROR_TYPE);
		}
		JSXAttribute::Shorthand(expr) => {
			checking_data.raise_unimplemented_error(
				"shorthand JSX attribute",
				expr.get_position().with_source(environment.get_source()),
			);
			return (PropertyKey::String(Cow::Borrowed("err")), TypeId::UNIMPLEMENTED_ERROR_TYPE);
		}
	};

	(PropertyKey::String(Cow::Owned(key.clone())), value)
}

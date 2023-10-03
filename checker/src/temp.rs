/// given `assertType` calls, displays the result
pub struct AssertTypeExplainer;

// impl Visitor<parser::Expression, TypeMappings> for AssertTypeExplainer {
// 	fn visit(
// 		&mut self,
// 		item: &parser::Expression,
// 		data: &mut TypeMappings,
// 		_chain: &parser::Chain,
// 	) {
// 		if let parser::Expression::FunctionCall { function, arguments, .. } = item {
// 			let ty = data
// 				.type_mappings
// 				.get_instance_for_expression(&function.get_expression_id().unwrap())
// 				.unwrap()
// 				.get_type();

// 			let function_calls = data.environment.get_constant_type(ty);

// 			if let Some(this_function_constant) = function_calls {
// 				let assert_type_function_constant: Constant =
// 					crate::InternalFunctionId::ASSERT_TYPE.into();
// 				let print_effects_function_constant: Constant =
// 					crate::InternalFunctionId::PRINT_EFFECTS.into();

// 				if this_function_constant == &assert_type_function_constant {
// 					print_arguments(arguments, data);
// 				} else if this_function_constant == &print_effects_function_constant {
// 					print_effects(arguments, data);
// 				}
// 			} else {
// 				eprintln!(
// 					"Calling thingy, maybe should have been replaced {:?}",
// 					data.environment.get_type_by_id(ty)
// 				);
// 			}
// 		}
// 	}
// }

// fn print_arguments(
// 	arguments: &Vec<parser::expressions::SpreadExpression>,
// 	data: &mut crate::AnalysisPassDataGroup,
// ) {
// 	let argument =
// 		if let Some(parser::expressions::SpreadExpression::NonSpread(expr)) = &arguments.first() {
// 			expr
// 		} else {
// 			unreachable!("type checking failed")
// 		};

// 	let instance =
// 		data.type_mappings.get_instance_for_expression(&argument.get_expression_id().unwrap());
// 	let diagnostic = if let Some(instance) = instance {
// 		let type_id = instance.get_type();
// 		let ty = data.environment.get_type_by_id(type_id);
// 		let mut value_as_string =
// 			TypeDisplay::to_string(&type_id, &data.environment.into_general_context());

// 		let specializations = data.environment.specializations.get(&type_id);
// 		if let Some(specializations) = specializations {
// 			value_as_string.push_str(" subsititued with ");
// 			for specialization in specializations.iter().copied() {
// 				let ty = data.environment.get_type_by_id(specialization);
// 				let specializations_as_string = TypeDisplay::to_string(
// 					&specialization,
// 					&data.environment.into_general_context(),
// 				);

// 				value_as_string.push_str(&specializations_as_string);
// 				value_as_string.push_str(", ");
// 			}
// 		}

// 		if let Some(Constant::FunctionReference(function_pointer)) =
// 			data.environment.get_constant_type(type_id)
// 		{
// 			crate::utils::notify!("TODO function references");
// 			Diagnostic::PositionWithAdditionLabels {
// 				labels: data
// 					.type_mappings
// 					.functions_to_positions
// 					.get(&function_pointer)
// 					.map(|span| (format!("Found function"), Some(span.clone())))
// 					.into_iter()
// 					.collect(),
// 				reason: format!("assertion passed with: {}", value_as_string),
// 				pos: argument.get_position().into_owned(),
// 			}
// 		} else {
// 			Diagnostic::Position {
// 				reason: format!("assertion passed with: {}", value_as_string),
// 				pos: argument.get_position().into_owned(),
// 			}
// 		}
// 	} else {
// 		Diagnostic::Position {
// 			reason: format!("assertion passed, however no type mapping present"),
// 			pos: argument.get_position().into_owned(),
// 		}
// 	};
// 	data.error_handler.add_info(diagnostic);
// }

// fn print_effects(
// 	arguments: &Vec<parser::expressions::SpreadExpression>,
// 	data: &mut crate::AnalysisPassDataGroup,
// ) {
// 	let argument =
// 		if let Some(parser::expressions::SpreadExpression::NonSpread(expr)) = &arguments.first() {
// 			expr
// 		} else {
// 			unreachable!("type checking failed")
// 		};

// 	let instance =
// 		data.type_mappings.get_instance_for_expression(&argument.get_expression_id().unwrap());
// 	let diagnostic = if let Some(instance) = instance {
// 		let type_id = instance.get_type();

// 		let specializations = data.environment.specializations.get(&type_id);
// 		if let Some(_specializations) = specializations {
// 			todo!()
// 		// value_as_string.push_str(" subsititued with ");
// 		// for specialization in specializations.iter().copied() {
// 		// 	let ty = data.environment.get_type_by_id(specialization);
// 		// 	let specializations_as_string = (specialization, ty)
// 		// 		.to_string(&data.environment.into_general_context());

// 		// 	value_as_string.push_str(&specializations_as_string);
// 		// 	value_as_string.push_str(", ");
// 		// }
// 		} else if let Some(function) = data.environment.get_function(type_id) {
// 			// TODO prettier result that verbose debug
// 			Diagnostic::Position {
// 				reason: format!("effects on function: {:#?}", function.effects),
// 				pos: argument.get_position().into_owned(),
// 			}
// 		} else {
// 			Diagnostic::Position {
// 				reason: "not a function wtf".into(),
// 				pos: argument.get_position().into_owned(),
// 			}
// 		}
// 	} else {
// 		unreachable!()
// 	};
// 	data.error_handler.add_info(diagnostic);
// }

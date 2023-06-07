use parser::BlockId;

use crate::{context::Environment, CheckingData};

/// Items include:
/// - Interfaces! (using statements for now)
pub(crate) fn declare_hoisted_items<T: crate::FSResolver>(
	checking_data: &mut CheckingData<T>,
	block_id: &BlockId,
	environment: &mut Environment,
) {
	todo!();
	// for interface in interfaces.iter() {
	// 	hoist_interface_name(&interface.on, environment, checking_data);
	// }

	// for interface in interfaces.iter() {
	// 	type_interface_declaration(interface, environment, checking_data);
	// }

	// for hoisted_function_id in
	// 	checking_data.functions.hoisted_functions.get(&block_id).into_iter().flatten()
	// {
	// 	let type_id = environment.new_function_type(
	// 		FunctionPointer::Function(parser::FunctionId::StatementFunctionBase(
	// 			*hoisted_function_id,
	// 		)),
	// 		&Default::default(),
	// 	);

	// 	let VariableIdentifier::Standard(name, variable_id, pos) = &function.name else { panic!() };

	// 	environment
	// 		.declare_variable(
	// 			name,
	// 			None,
	// 			type_id,
	// 			*variable_id,
	// 			checking_data.settings.constant_function_declarations,
	// 			pos.clone(),
	// 		)
	// 		.expect("TODO re-declared variable error");

	// 	crate::utils::notify!("TODO temp statement function prototype error");

	// 	checking_data
	// 		.type_mappings
	// 		.statement_functions_to_prototypes
	// 		.insert(*hoisted_function_id, TypeId::ERROR_TYPE);

	// 	// if is_exported {
	// 	// 	checking_data.error_warning_info_handler.add_warning(
	// 	// 		TypeCheckWarnings::Unsupported {
	// 	// 			thing: "export (treating as un-exported for now)",
	// 	// 			at: function.get_position().clone(),
	// 	// 		},
	// 	// 	);
	// 	// }
	// }
}

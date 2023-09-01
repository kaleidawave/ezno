use crate::{
	types::functions::SynthesizedArgument,
	types::{printing::print_type, Type, TypeStore},
	Constant, Environment, TypeId,
};

// TODO ...
pub(crate) enum ConstantResult {
	Value(TypeId),
	Diagnostic(String),
}

/// Computes a constant value
pub(crate) fn call_constant_function(
	id: &str,
	this_arg: Option<TypeId>,
	arguments: &[SynthesizedArgument],
	types: &mut TypeStore,
	environment: &Environment,
) -> Result<ConstantResult, ()> {
	// crate::utils::notify!("Calling constant function {} with {:?}", name, arguments);
	// TODO as parameter
	match id {
		// Single parameter number functions
		"sin" | "cos" | "tan" | "atan" | "acos" | "asin" | "sinh" | "cosh" | "tanh" | "asinh"
		| "acosh" | "atanh" | "exp" | "expm1" | "log" | "log10" | "log2" | "log1p" | "round"
		| "trunc" | "sqrt" | "cbrt" | "abs" => {
			let second_argument_type =
				types.get_type_by_id(arguments.last().unwrap().into_type().unwrap());

			let Type::Constant(Constant::Number(num)) = second_argument_type else { return Err(()) };

			let result = match id {
				"sin" => num.sin(),
				"cos" => num.cos(),
				"tan" => num.tan(),
				"atan" => num.atan(),
				"acos" => num.acos(),
				"asin" => num.asin(),
				"sinh" => num.sinh(),
				"cosh" => num.cosh(),
				"tanh" => num.tanh(),
				"asinh" => num.asinh(),
				"acosh" => num.acosh(),
				"atanh" => num.atanh(),
				"exp" => num.exp(),
				"expm1" => num.exp_m1(),
				"log" => num.ln(),
				"log10" => num.log10(),
				"log2" => num.log2(),
				"log1p" => num.ln_1p(),
				"round" => num.round(),
				"trunc" => num.trunc(),
				"sqrt" => num.sqrt(),
				"cbrt" => num.cbrt(),
				"abs" => num.abs(),
				_ => unreachable!(),
			};

			let try_into = result.try_into();
			match try_into {
				Ok(try_into) => {
					Ok(ConstantResult::Value(types.new_constant_type(Constant::Number(try_into))))
				}
				Err(_) => return Ok(ConstantResult::Value(TypeId::NAN_TYPE)),
			}
		}
		"uppercase" | "lowercase" => {
			if let Type::Constant(Constant::String(s)) = types.get_type_by_id(this_arg.unwrap()) {
				let result = types.new_constant_type(Constant::String(match id {
					"uppercase" => s.to_uppercase(),
					"lowercase" => s.to_lowercase(),
					_ => unreachable!(),
				}));
				Ok(ConstantResult::Value(result))
			} else {
				Err(())
			}
		}
		"print_type" | "debug_type" => {
			let debug = id == "debug_type";
			let ty = arguments.first().unwrap().into_type().unwrap();
			let ty_as_string = print_type(ty, types, &environment.into_general_context(), debug);
			Ok(ConstantResult::Diagnostic(format!("Type is: {ty_as_string}")))
		}
		"debug_effects" => {
			let ty = arguments.first().unwrap().into_type().unwrap();
			if let Type::Function(func, _) = types.get_type_by_id(ty) {
				// TODO actual values
				Ok(ConstantResult::Diagnostic(format!("{:#?}", func.effects)))
			} else {
				Ok(ConstantResult::Diagnostic("not a function".to_owned()))
			}
		}
		"debug_context" => Ok(ConstantResult::Diagnostic(environment.debug())),
		"is_dependent" => Ok(ConstantResult::Diagnostic(format!(
			"is dependent {:?}",
			types.get_type_by_id(arguments.first().unwrap().into_type().unwrap()).is_dependent()
		))),
		func => panic!("Unknown/unimplemented const function {func}"),
	}
}

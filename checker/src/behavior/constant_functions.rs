use source_map::{Span, SpanWithSource};

use crate::{
	context::get_on_ctx,
	subtyping::check_satisfies,
	types::{
		functions::SynthesisedArgument, poly_types::generic_type_arguments::TypeArgumentStore,
	},
	types::{poly_types::FunctionTypeArguments, printing::print_type, Type, TypeStore},
	Constant, Environment, TypeId,
};

use super::functions::ThisValue;

// TODO ...
pub(crate) enum ConstantResult {
	Value(TypeId),
	Diagnostic(String),
}

/// Computes a constant value
pub(crate) fn call_constant_function(
	id: &str,
	this_argument: ThisValue,
	call_site_type_args: &Option<Vec<(SpanWithSource, TypeId)>>,
	arguments: &[SynthesisedArgument],
	types: &mut TypeStore,
	// TODO mut for satisfies which needs checking
	environment: &mut Environment,
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

			let Type::Constant(Constant::Number(num)) = second_argument_type else {
				return Err(());
			};

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
					let ty = types.new_constant_type(Constant::Number(try_into));
					crate::utils::notify!("{:?}", ty);
					Ok(ConstantResult::Value(ty))
				}
				Err(_) => Ok(ConstantResult::Value(TypeId::NAN_TYPE)),
			}
		}
		"uppercase" | "lowercase" => {
			crate::utils::notify!("this_argument = {:?}", this_argument);
			if let Type::Constant(Constant::String(s)) =
				types.get_type_by_id(this_argument.unwrap())
			{
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
		// TODO second argument Rust
		"print_type" | "debug_type" => {
			let debug = id == "debug_type";
			let ty = arguments.first().unwrap().into_type().unwrap();
			let ty_as_string = print_type(ty, types, &environment.into_general_context(), debug);
			Ok(ConstantResult::Diagnostic(format!("Type is: {ty_as_string}")))
		}
		"debug_effects" => {
			let ty = arguments.first().unwrap().into_type().unwrap();
			if let Type::Function(func, _) = types.get_type_by_id(ty) {
				// TODO print using debug
				let effects = &types.functions.get(func).unwrap().effects;
				Ok(ConstantResult::Diagnostic(format!("{:#?}", effects)))
			} else {
				Ok(ConstantResult::Diagnostic("not a function".to_owned()))
			}
		}
		// For functions
		"call" | "bind" => {
			todo!()
		}
		"satisfies" => {
			let ty = arguments.first().unwrap().into_type().unwrap();
			// TODO temp!!!
			let arg = call_site_type_args.iter().flatten().next().unwrap().1;
			if check_satisfies(arg, ty, types, environment) {
				Ok(ConstantResult::Value(ty))
			} else {
				Ok(ConstantResult::Diagnostic(format!(
					"Expected {}, found {}",
					print_type(ty, types, &environment.into_general_context(), false),
					print_type(arg, types, &environment.into_general_context(), false)
				)))
			}
		}
		"debug_context" => Ok(ConstantResult::Diagnostic(environment.debug())),
		"context_id" => Ok(ConstantResult::Diagnostic(format!("in {:?}", environment.context_id))),
		"context_id_chain" => Ok(ConstantResult::Diagnostic({
			use std::fmt::Write;
			let mut buf = format!("{:?}", environment.context_id);
			for ctx in environment.parents_iter().skip(1) {
				write!(&mut buf, " <- {:?}", get_on_ctx!(ctx.context_id)).unwrap();
			}
			buf
		})),
		"is_dependent" => Ok(ConstantResult::Diagnostic(format!(
			"is dependent {:?}",
			types.get_type_by_id(arguments.first().unwrap().into_type().unwrap()).is_dependent()
		))),
		func => panic!("Unknown/unimplemented const function {func}"),
	}
}

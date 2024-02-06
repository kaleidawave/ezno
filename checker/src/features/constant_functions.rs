use crate::{
	context::{get_on_ctx, information::InformationChain},
	// subtyping::check_satisfies,
	types::{
		functions::SynthesisedArgument, printing::debug_effects, Constructor, StructureGenerics,
		TypeRestrictions,
	},
	types::{printing::print_type, Type, TypeStore},
	Constant,
	Environment,
	TypeId,
};

use super::functions::ThisValue;

// TODO ...
pub(crate) enum ConstantOutput {
	Value(TypeId),
	Diagnostic(String),
}

pub enum ConstantFunctionError {
	NoLogicForIdentifier(String),
	/// This will get picked up by the main calling logic
	BadCall,
}

/// From when
/// ```typescript
/// f<number>(2, 3)
///   ^^^^^^
/// ```
/// is called
pub(crate) type CallSiteTypeArguments = TypeRestrictions;

/// Computes a constant value
pub(crate) fn call_constant_function(
	id: &str,
	this_argument: ThisValue,
	_call_site_type_args: Option<&CallSiteTypeArguments>,
	arguments: &[SynthesisedArgument],
	types: &mut TypeStore,
	// TODO mut for satisfies which needs checking
	environment: &mut Environment,
) -> Result<ConstantOutput, ConstantFunctionError> {
	// crate::utils::notify!("Calling constant function {} with {:?}", name, arguments);
	// TODO as parameter
	match id {
		// Single parameter number functions
		"sin" | "cos" | "tan" | "atan" | "acos" | "asin" | "sinh" | "cosh" | "tanh" | "asinh"
		| "acosh" | "atanh" | "exp" | "expm1" | "log" | "log10" | "log2" | "log1p" | "round"
		| "floor" | "ceil" | "trunc" | "sqrt" | "cbrt" | "abs" => {
			let second_argument_type =
				types.get_type_by_id(arguments.last().ok_or(ConstantFunctionError::BadCall)?.value);

			let Type::Constant(Constant::Number(num)) = second_argument_type else {
				return Err(ConstantFunctionError::BadCall);
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
				"floor" => num.floor(),
				"ceil" => num.ceil(),
				"sqrt" => num.sqrt(),
				"cbrt" => num.cbrt(),
				"abs" => num.abs(),
				_ => unreachable!(),
			};

			let try_into = result.try_into();
			match try_into {
				Ok(try_into) => {
					let ty = types.new_constant_type(Constant::Number(try_into));
					Ok(ConstantOutput::Value(ty))
				}
				Err(_) => Ok(ConstantOutput::Value(TypeId::NAN_TYPE)),
			}
		}
		"uppercase" | "lowercase" | "string_length" => {
			if let Some(Type::Constant(Constant::String(s))) =
				this_argument.get_passed().map(|t| types.get_type_by_id(t))
			{
				let result = types.new_constant_type(match id {
					"uppercase" => Constant::String(s.to_uppercase()),
					"lowercase" => Constant::String(s.to_lowercase()),
					"string_length" => Constant::Number(
						(s.len() as f64).try_into().map_err(|_| ConstantFunctionError::BadCall)?,
					),
					_ => unreachable!(),
				});
				Ok(ConstantOutput::Value(result))
			} else {
				// This can occur!!
				Err(ConstantFunctionError::BadCall)
			}
		}
		"print_type" | "debug_type" | "print_and_debug_type" | "debug_type_independent" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;

			let print = id.contains("print").then(|| print_type(ty, types, environment, false));
			let debug = id.contains("debug").then(|| print_type(ty, types, environment, true));

			let result = match (print, debug) {
				(Some(print), Some(debug)) => format!("Type is: {print} / {debug}"),
				(None, Some(out)) | (Some(out), None) => format!("Type is: {out}"),
				(None, None) => unreachable!(),
			};

			Ok(ConstantOutput::Diagnostic(result))
		}
		"print_constraint" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;

			let constraint = environment
				.get_chain_of_info()
				.find_map(|i| i.object_constraints.get(&ty).cloned());

			if let Some(constraint) = constraint {
				let constraint_as_string = print_type(constraint, types, environment, false);
				Ok(ConstantOutput::Diagnostic(format!("Constraint is: {constraint_as_string}")))
			} else {
				Ok(ConstantOutput::Diagnostic("No associate constraint".to_owned()))
			}
		}
		"debug_type_rust" | "debug_type_rust_independent" => {
			let id = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;

			let ty = types.get_type_by_id(id);
			Ok(ConstantOutput::Diagnostic(format!("Type is: {id:?} = {ty:?}")))
		}
		"debug_effects" | "debug_effects_rust" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;

			// Unwrap structure generics
			let ty = if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
				on,
				..
			})) = types.get_type_by_id(ty)
			{
				*on
			} else {
				ty
			};

			let get_type_by_id = types.get_type_by_id(ty);
			if let Type::Function(func, _) | Type::FunctionReference(func) = get_type_by_id {
				let effects =
					&types.functions.get(func).ok_or(ConstantFunctionError::BadCall)?.effects;
				if id.ends_with("rust") {
					Ok(ConstantOutput::Diagnostic(format!("{effects:#?}")))
				} else {
					let mut buf = String::new();
					debug_effects(&mut buf, effects, types, environment, true);
					Ok(ConstantOutput::Diagnostic(buf))
				}
			} else {
				Ok(ConstantOutput::Diagnostic(format!("{get_type_by_id:?} is not a function")))
			}
		}
		// For functions
		"bind" => {
			let on = this_argument.get_passed().map(|t| types.get_type_by_id(t));
			let first_argument = arguments.first();
			if let (Some(Type::Function(func, _) | Type::FunctionReference(func)), Some(this_ty)) =
				(on, first_argument)
			{
				let type_id =
					this_ty.non_spread_type().map_err(|()| ConstantFunctionError::BadCall)?;
				let value = types.register_type(Type::Function(*func, ThisValue::Passed(type_id)));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		"set_prototype" => {
			if let [first, second] = arguments {
				let _prototype = environment
					.info
					.prototypes
					.insert(first.non_spread_type().unwrap(), second.non_spread_type().unwrap());
				// TODO
				Ok(ConstantOutput::Value(TypeId::UNDEFINED_TYPE))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		"get_prototype" => {
			if let Some(first) = arguments.first() {
				crate::utils::notify!("TODO walk up chain");
				let prototype = environment
					.info
					.prototypes
					.get(&first.non_spread_type().unwrap())
					.copied()
					.unwrap_or(TypeId::NULL_TYPE);
				Ok(ConstantOutput::Value(prototype))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		"create_proxy" => {
			if let [object, trap] = arguments {
				// TODO checking for both, what about spreading
				let value = types.register_type(Type::SpecialObject(
					crate::features::objects::SpecialObjects::Proxy {
						handler: trap.non_spread_type().expect("single type"),
						over: object.non_spread_type().expect("single type"),
					},
				));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		// "satisfies" => {
		// 	let ty = arguments
		// 		.first()
		// 		.ok_or(ConstantFunctionError::BadCall)?
		// 		.non_spread_type()
		// 		.map_err(|()| ConstantFunctionError::BadCall)?;
		// 	// TODO temp!!!
		// 	let arg = call_site_type_args
		// 		.iter()
		// 		.flatten()
		// 		.next()
		// 		.ok_or(ConstantFunctionError::BadCall)?
		// 		.0;
		// 	if check_satisfies(arg, ty, types, environment) {
		// 		Ok(ConstantOutput::Value(ty))
		// 	} else {
		// 		let output = format!(
		// 			"Expected {}, found {}",
		// 			print_type(ty, types, environment, false),
		// 			print_type(arg, types, environment, false)
		// 		);
		// 		Ok(ConstantOutput::Diagnostic(output))
		// 	}
		// }
		"debug_context" => Ok(ConstantOutput::Diagnostic(environment.debug())),
		"context_id" => Ok(ConstantOutput::Diagnostic(format!("in {:?}", environment.context_id))),
		"context_id_chain" => Ok(ConstantOutput::Diagnostic({
			use std::fmt::Write;
			let mut buf = format!("{:?}", environment.context_id);
			for ctx in environment.parents_iter().skip(1) {
				write!(&mut buf, " <- {:?}", get_on_ctx!(ctx.context_id))
					.map_err(|_| ConstantFunctionError::BadCall)?;
			}
			buf
		})),
		"is_dependent" => Ok(ConstantOutput::Diagnostic(format!(
			"is dependent {:?}",
			types
				.get_type_by_id(
					arguments
						.first()
						.ok_or(ConstantFunctionError::BadCall)?
						.non_spread_type()
						.map_err(|()| ConstantFunctionError::BadCall)?
				)
				.is_dependent()
		))),
		// "compile_type_to_object" => {
		// 	if let Some(value) = call_site_type_args {
		// 		let value = crate::types::others::create_object_for_type(
		// 			value.first().ok_or(ConstantFunctionError::BadCall)?.0,
		// 			environment,
		// 			types,
		// 		);
		// 		Ok(ConstantOutput::Value(value))
		// 	} else {
		// 		Err(ConstantFunctionError::BadCall)
		// 	}
		// }
		func => {
			// Sometimes a bad definition file could occur so...
			Err(ConstantFunctionError::NoLogicForIdentifier(func.to_owned()))
		}
	}
}

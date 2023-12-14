use source_map::{Span, SpanWithSource};

use crate::{
	context::get_on_ctx,
	subtyping::check_satisfies,
	types::{
		functions::SynthesisedArgument, poly_types::generic_type_arguments::TypeArgumentStore,
		printing::debug_effects,
	},
	types::{poly_types::FunctionTypeArguments, printing::print_type, Type, TypeStore},
	Constant, Environment, TypeId,
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

/// Computes a constant value
pub(crate) fn call_constant_function(
	id: &str,
	this_argument: ThisValue,
	call_site_type_args: &Option<Vec<(TypeId, SpanWithSource)>>,
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
			let second_argument_type = types.get_type_by_id(
				arguments
					.last()
					.ok_or(ConstantFunctionError::BadCall)?
					.to_type()
					.map_err(|()| ConstantFunctionError::BadCall)?,
			);

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
				Err(ConstantFunctionError::BadCall)
			}
		}
		// TODO second argument Rust
		"print_type" | "debug_type" => {
			let debug = id == "debug_type";
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.to_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;
			let ty_as_string = print_type(ty, types, &environment.as_general_context(), debug);
			Ok(ConstantOutput::Diagnostic(format!("Type is: {ty_as_string}")))
		}
		"debug_effects_rust" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.to_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;
			if let Type::Function(func, _) | Type::FunctionReference(func, _) =
				types.get_type_by_id(ty)
			{
				let effects =
					&types.functions.get(func).ok_or(ConstantFunctionError::BadCall)?.effects;
				Ok(ConstantOutput::Diagnostic(format!("{effects:#?}")))
			} else {
				Ok(ConstantOutput::Diagnostic("not a function".to_owned()))
			}
		}
		"debug_effects" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.to_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;
			if let Type::Function(func, _) | Type::FunctionReference(func, _) =
				types.get_type_by_id(ty)
			{
				let effects =
					&types.functions.get(func).ok_or(ConstantFunctionError::BadCall)?.effects;
				let mut buf = String::new();
				debug_effects(&mut buf, effects, types, &environment.as_general_context(), true);
				Ok(ConstantOutput::Diagnostic(buf))
			} else {
				Ok(ConstantOutput::Diagnostic("not a function".to_owned()))
			}
		}
		// For functions
		"bind" => {
			let on = this_argument.get_passed().map(|t| types.get_type_by_id(t));
			let first_argument = arguments.first();
			if let (
				Some(Type::Function(func, _) | Type::FunctionReference(func, _)),
				Some(this_ty),
			) = (on, first_argument)
			{
				let type_id = this_ty.to_type().map_err(|()| ConstantFunctionError::BadCall)?;
				let value = types.register_type(Type::Function(*func, ThisValue::Passed(type_id)));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		"set_prototype" => {
			if let [first, second] = arguments {
				let prototype = environment
					.facts
					.prototypes
					.insert(first.to_type().unwrap(), second.to_type().unwrap());
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
					.facts
					.prototypes
					.get(&first.to_type().unwrap())
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
					crate::behavior::objects::SpecialObjects::Proxy {
						handler: trap.to_type().expect("single type"),
						over: object.to_type().expect("single type"),
					},
				));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		"satisfies" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::BadCall)?
				.to_type()
				.map_err(|()| ConstantFunctionError::BadCall)?;
			// TODO temp!!!
			let arg = call_site_type_args
				.iter()
				.flatten()
				.next()
				.ok_or(ConstantFunctionError::BadCall)?
				.0;
			if check_satisfies(arg, ty, types, environment) {
				Ok(ConstantOutput::Value(ty))
			} else {
				let output = format!(
					"Expected {}, found {}",
					print_type(ty, types, &environment.as_general_context(), false),
					print_type(arg, types, &environment.as_general_context(), false)
				);
				Ok(ConstantOutput::Diagnostic(output))
			}
		}
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
						.to_type()
						.map_err(|()| ConstantFunctionError::BadCall)?
				)
				.is_dependent()
		))),
		"compile_type_to_object" => {
			if let Some(value) = call_site_type_args {
				let value = crate::types::others::create_object_for_type(
					value.first().ok_or(ConstantFunctionError::BadCall)?.0,
					environment,
					types,
				);
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::BadCall)
			}
		}
		func => {
			// Sometimes a bad definition file could occur so...
			Err(ConstantFunctionError::NoLogicForIdentifier(func.to_owned()))
		}
	}
}

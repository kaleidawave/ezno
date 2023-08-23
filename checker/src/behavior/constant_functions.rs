use crate::{
	types::functions::SynthesizedArgument,
	types::{cast_as_number, cast_as_string, printing::print_type, Type, TypeStore},
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
	context: &mut Environment,
) -> Result<ConstantResult, ()> {
	// crate::utils::notify!("Calling constant function {} with {:?}", name, arguments);
	// TODO as parameter
	const STRICT_CASTS: bool = false;

	match id {
		"sin" | "cos" | "tan" => {
			let second_argument_type =
				types.get_type_by_id(arguments.last().unwrap().into_type().unwrap());

			let Type::Constant(Constant::Number(num)) = second_argument_type else { return Err(()) };

			let result = match id {
				"sin" => num.sin(),
				"cos" => num.cos(),
				"tan" => num.tan(),
				_ => unreachable!(),
			};

			let ty = types.new_constant_type(Constant::Number(result.try_into().unwrap()));
			Ok(ConstantResult::Value(ty))
		}
		// TODO sub temp
		"add" | "sub" => {
			let (first, second) = {
				let mut iter = arguments
					.iter()
					.map(|arg| arg.into_type().unwrap())
					.map(|ty| types.get_type_by_id(ty));

				(iter.next().unwrap(), iter.next().unwrap())
			};

			match (first, second) {
				(Type::Constant(Constant::Number(a)), Type::Constant(Constant::Number(b))) => {
					let ty = if id == "add" {
						types.new_constant_type(Constant::Number(a + b))
					} else {
						types.new_constant_type(Constant::Number(a - b))
					};
					Ok(ConstantResult::Value(ty))
				}
				(Type::Constant(c1), Type::Constant(c2)) => {
					let ty = if id == "add" {
						// TODO temp
						let result = format!(
							"{}{}",
							cast_as_string(&c1, STRICT_CASTS).unwrap(),
							cast_as_string(&c2, STRICT_CASTS).unwrap()
						);
						types.new_constant_type(Constant::String(result))
					} else {
						let lhs = cast_as_number(c1, STRICT_CASTS).unwrap_or(f64::NAN);
						let rhs = cast_as_number(c1, STRICT_CASTS).unwrap_or(f64::NAN);
						let value = ordered_float::NotNan::try_from(lhs - rhs);
						match value {
							Ok(value) => types.new_constant_type(Constant::Number(value)),
							Err(_) => TypeId::NAN_TYPE,
						}
					};
					Ok(ConstantResult::Value(ty))
				}
				_ => return Err(()),
			}
		}
		"mul" => {
			let (first, second) = {
				let mut iter = arguments
					.iter()
					.map(|arg| arg.into_type().unwrap())
					.map(|ty| types.get_type_by_id(ty));

				(iter.next().unwrap(), iter.next().unwrap())
			};

			match (first, second) {
				(Type::Constant(c1), Type::Constant(c2)) => {
					let lhs = cast_as_number(c1, STRICT_CASTS).unwrap_or(f64::NAN);
					let rhs = cast_as_number(c1, STRICT_CASTS).unwrap_or(f64::NAN);
					let value = ordered_float::NotNan::try_from(lhs * rhs);
					let ty = match value {
						Ok(value) => types.new_constant_type(Constant::Number(value)),
						Err(_) => TypeId::NAN_TYPE,
					};
					Ok(ConstantResult::Value(ty))
				}
				_ => return Err(()),
			}
		}
		"equal" => {
			let (first, second) = {
				let mut iter = arguments.iter().map(|arg| arg.into_type().unwrap());

				(iter.next().unwrap(), iter.next().unwrap())
			};

			let are_equal = if first == second {
				true
			} else if let (Type::Constant(cst1), Type::Constant(cst2)) =
				(types.get_type_by_id(first), types.get_type_by_id(second))
			{
				cst1 == cst2
			} else {
				// TODO also Err if unknown
				false
			};
			Ok(ConstantResult::Value(types.new_constant_type(Constant::Boolean(are_equal))))
		}
		"lowercase" => {
			if let Type::Constant(Constant::String(s)) = types.get_type_by_id(this_arg.unwrap()) {
				let lowercase_ty = types.new_constant_type(Constant::String(s.to_lowercase()));
				Ok(ConstantResult::Value(lowercase_ty))
			} else {
				Err(())
			}
		}
		"uppercase" => {
			if let Type::Constant(Constant::String(s)) = types.get_type_by_id(this_arg.unwrap()) {
				let uppercase_ty = types.new_constant_type(Constant::String(s.to_uppercase()));
				Ok(ConstantResult::Value(uppercase_ty))
			} else {
				Err(())
			}
		}
		"print_type" | "debug_type" => {
			let debug = id == "debug_type";
			let ty = arguments.first().unwrap().into_type().unwrap();
			let ty_as_string = print_type(ty, types, &context.into_general_context(), debug);
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
		"debug_context" => Ok(ConstantResult::Diagnostic(context.debug())),
		func => panic!("Unknown/unimplemented const function {func}"),
	}
}

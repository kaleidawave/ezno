use crate::{
	types::functions::SynthesizedArgument,
	types::{cast_as_string, Type, TypeStore},
	Constant, TypeId,
};

/// Computes a constant value
pub(crate) fn call_constant_function(
	name: &str,
	this_arg: Option<TypeId>,
	arguments: &[SynthesizedArgument],
	types: &mut TypeStore,
) -> Result<TypeId, ()> {
	// crate::utils::notify!("Calling constant function {} with {:?}", name, arguments);

	match name {
		"sin" | "cos" | "tan" => {
			let second_argument_type =
				types.get_type_by_id(arguments.last().unwrap().into_type().unwrap());

			let Type::Constant(Constant::Number(num)) = second_argument_type else { return Err(()) };

			let result = match name {
				"sin" => num.sin(),
				"cos" => num.cos(),
				"tan" => num.tan(),
				_ => unreachable!(),
			};

			Ok(types.new_constant_type(Constant::Number(result.try_into().unwrap())))
		}
		"add" => {
			let (first, second) = {
				let mut iter = arguments
					.iter()
					.map(|arg| arg.into_type().unwrap())
					.map(|ty| types.get_type_by_id(ty));

				(iter.next().unwrap(), iter.next().unwrap())
			};

			match (first, second) {
				(Type::Constant(Constant::Number(a)), Type::Constant(Constant::Number(b))) => {
					Ok(types.new_constant_type(Constant::Number(a + b)))
				}
				(Type::Constant(c1), Type::Constant(c2)) => {
					// TODO temp
					let result = format!(
						"{}{}",
						cast_as_string(&c1, false).unwrap(),
						cast_as_string(&c2, false).unwrap()
					);
					Ok(types.new_constant_type(Constant::String(result)))
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
				(Type::Constant(Constant::Number(a)), Type::Constant(Constant::Number(b))) => {
					Ok(types.new_constant_type(Constant::Number(a * b)))
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
			Ok(types.new_constant_type(Constant::Boolean(are_equal)))
		}
		"uppercase" => {
			if let Type::Constant(Constant::String(s)) = types.get_type_by_id(this_arg.unwrap()) {
				Ok(types.new_constant_type(Constant::String(s.to_uppercase())))
			} else {
				Err(())
			}
		}
		func => panic!("Unknown/unimplemented const function {func}"),
	}
}

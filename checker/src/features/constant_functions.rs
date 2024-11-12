use iterator_endiate::EndiateIteratorExt;
use source_map::SpanWithSource;

use crate::{
	context::{get_on_ctx, information::InformationChain, invocation::CheckThings},
	events::printing::debug_effects,
	features::objects::{ObjectBuilder, Proxy},
	types::{
		calling::{
			Callable, CallingDiagnostics, FunctionCallingError, SynthesisedArgument, ThisValue,
		},
		logical::{Logical, LogicalOrValid},
		printing::print_type,
		properties::{AccessMode, Descriptor, PropertyKey, Publicity},
		FunctionEffect, PartiallyAppliedGenerics, Type, TypeRestrictions, TypeStore,
	},
	Constant, Environment, PropertyValue, TypeId,
};

use super::objects::SpecialObject;

// TODO ...
pub(crate) enum ConstantOutput {
	Value(TypeId),
	Diagnostic(String),
}

pub enum ConstantFunctionError {
	FunctionCallingError(FunctionCallingError),
	NoLogicForIdentifier(String),
	/// This will get picked up by the main calling logic
	CannotComputeConstant,
}

/// From when
/// ```typescript
/// f<number>(2, 3)
///   ^^^^^^
/// ```
/// is called
pub(crate) type CallSiteTypeArguments = TypeRestrictions;

/// Computes a constant value
#[allow(clippy::too_many_arguments)]
pub(crate) fn call_constant_function(
	id: &str,
	this_argument: ThisValue,
	call_site_type_args: Option<&CallSiteTypeArguments>,
	arguments: &[SynthesisedArgument],
	types: &mut TypeStore,
	// TODO `mut` for satisfies which needs checking. Also needed for freeze etc
	environment: &mut Environment,
	call_site: SpanWithSource,
	diagnostics: &mut CallingDiagnostics,
) -> Result<ConstantOutput, ConstantFunctionError> {
	// crate::utilities::notify!("Calling constant function {} with {:?}", name, arguments);
	// TODO as parameter
	match id {
		// Single parameter number functions
		"sin" | "cos" | "tan" | "atan" | "acos" | "asin" | "sinh" | "cosh" | "tanh" | "asinh"
		| "acosh" | "atanh" | "exp" | "expm1" | "log" | "log10" | "log2" | "log1p" | "round"
		| "floor" | "ceil" | "trunc" | "sqrt" | "cbrt" | "abs" => {
			if arguments.len() > 1 {
				return Err(ConstantFunctionError::CannotComputeConstant);
			}

			let first_argument = types.get_type_by_id(
				arguments.last().ok_or(ConstantFunctionError::CannotComputeConstant)?.value,
			);

			let Type::Constant(Constant::Number(num)) = first_argument else {
				return Err(ConstantFunctionError::CannotComputeConstant);
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

			let Ok(num) = result.try_into() else { return Ok(ConstantOutput::Value(TypeId::NAN)) };
			Ok(ConstantOutput::Value(types.new_constant_type(Constant::Number(num))))
		}
		"imul" => {
			if let [x, y] = arguments {
				if let (Type::Constant(Constant::Number(x)), Type::Constant(Constant::Number(y))) =
					(types.get_type_by_id(x.value), types.get_type_by_id(y.value))
				{
					// TODO is this correct, what about overflow?
					let result = (x.into_inner() as i32) * (y.into_inner() as i32);
					Ok(ConstantOutput::Value(
						types.new_constant_type(Constant::Number(result.into())),
					))
				} else {
					Err(ConstantFunctionError::CannotComputeConstant)
				}
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		// String stuff. TODO could this be replaced by intrinsics
		"toUpperCase" | "toLowerCase" | "string_length" => {
			if let Some(Type::Constant(Constant::String(s))) =
				this_argument.get_passed().map(|t| types.get_type_by_id(t))
			{
				let result = types.new_constant_type(match id {
					"toUpperCase" => Constant::String(s.to_uppercase()),
					"toLowerCase" => Constant::String(s.to_lowercase()),
					"string_length" => Constant::Number(
						(s.encode_utf16().count() as f64)
							.try_into()
							.map_err(|_| ConstantFunctionError::CannotComputeConstant)?,
					),
					_ => unreachable!(),
				});
				Ok(ConstantOutput::Value(result))
			} else {
				// This can occur!!
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"debug_number" => {
			let arg = arguments.iter().next().unwrap();
			Ok(ConstantOutput::Diagnostic(format!(
				"number: {:?}",
				crate::types::intrinsics::get_range_and_mod_class(arg.value, types)
			)))
		}
		"print_type" | "debug_type" | "print_and_debug_type" | "debug_type_independent" => {
			fn to_string(
				print: bool,
				debug: bool,
				ty: TypeId,
				types: &TypeStore,
				environment: &Environment,
			) -> String {
				let print = print.then(|| print_type(ty, types, environment, false));
				let debug = debug.then(|| print_type(ty, types, environment, true));

				match (print, debug) {
					(Some(print), Some(debug)) => format!("{print} / {debug}"),
					(None, Some(out)) | (Some(out), None) => out,
					(None, None) => unreachable!(),
				}
			}

			let print = id.contains("print");
			let debug = id.contains("debug");

			if let Some(arg) = call_site_type_args {
				let (arg, _pos) = arg.values().next().unwrap();
				let mut buf = String::from("Types: ");
				buf.push_str(&to_string(print, debug, *arg, types, environment));
				Ok(ConstantOutput::Diagnostic(buf))
			} else {
				let mut buf = String::from("Types: ");
				for (not_at_end, arg) in arguments.iter().nendiate() {
					// crate::utilities::notify!("at end {:?} {:?}", not_at_end, arg);
					let arg = arg
						.non_spread_type()
						.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
					buf.push_str(&to_string(print, debug, arg, types, environment));
					if not_at_end {
						buf.push_str(", ");
					}
				}
				Ok(ConstantOutput::Diagnostic(buf))
			}
		}
		"print_constraint" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::CannotComputeConstant)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;

			let constraint = environment
				.get_chain_of_info()
				.find_map(|i| i.object_constraints.get(&ty).copied());

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
				.ok_or(ConstantFunctionError::CannotComputeConstant)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;

			let ty = types.get_type_by_id(id);
			Ok(ConstantOutput::Diagnostic(format!("Type is: {id:?} = {ty:?}")))
		}
		"debug_effects" | "debug_effects_rust" => {
			let ty = arguments
				.first()
				.ok_or(ConstantFunctionError::CannotComputeConstant)?
				.non_spread_type()
				.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;

			// Unwrap structure generics
			let ty =
				if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, .. }) =
					types.get_type_by_id(ty)
				{
					*on
				} else {
					ty
				};

			let get_type_by_id = types.get_type_by_id(ty);
			let message = if let Type::SpecialObject(SpecialObject::Function(func, _))
			| Type::FunctionReference(func) = get_type_by_id
			{
				let function_type = types
					.functions
					.get(func)
					.ok_or(ConstantFunctionError::CannotComputeConstant)?;

				let effects = &function_type.effect;
				if id.ends_with("rust") {
					format!("{effects:#?}")
				} else {
					match effects {
						FunctionEffect::SideEffects { events, .. } => {
							let mut buf = String::from("Effects:\n");
							debug_effects(&mut buf, events, types, environment, 0, true);
							buf
						}
						FunctionEffect::Constant { identifier, may_throw: _ } => {
							format!("Constant: {identifier}")
						}
						FunctionEffect::InputOutput { identifier, may_throw: _ } => {
							format!("InputOutput: {identifier}")
						}
						FunctionEffect::Unknown => "unknown".into(),
					}
				}
			} else {
				format!("{get_type_by_id:?} is not a function")
			};
			Ok(ConstantOutput::Diagnostic(message))
		}
		// For functions
		"bind" => {
			let on = this_argument.get_passed().map(|t| types.get_type_by_id(t));
			let first_argument = arguments.first();
			if let (
				Some(
					Type::SpecialObject(SpecialObject::Function(func, _))
					| Type::FunctionReference(func),
				),
				Some(this_ty),
			) = (on, first_argument)
			{
				let type_id = this_ty
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
				let value = types.register_type(Type::SpecialObject(SpecialObject::Function(
					*func,
					ThisValue::Passed(type_id),
				)));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"setPrototypeOf" => {
			if let [first, second] = arguments {
				if let (Ok(first), Ok(second)) = (first.non_spread_type(), second.non_spread_type())
				{
					let _prototype = environment.info.prototypes.insert(first, second);

					crate::utilities::notify!("Set {:?} prototype to {:?}", first, second);

					Ok(ConstantOutput::Value(first))
				} else {
					Err(ConstantFunctionError::CannotComputeConstant)
				}
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"getPrototypeOf" => {
			if let Some(first) = arguments.first() {
				let on = first.non_spread_type().unwrap();
				let prototype = environment.get_prototype(on);
				Ok(ConstantOutput::Value(prototype))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"freeze" => {
			if let Some(on) =
				(arguments.len() == 1).then(|| arguments[0].non_spread_type().ok()).flatten()
			{
				environment.info.frozen.insert(on);
				Ok(ConstantOutput::Value(on))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"isFrozen" => {
			if let Some(on) =
				(arguments.len() == 1).then(|| arguments[0].non_spread_type().ok()).flatten()
			{
				let is_frozen =
					environment.get_chain_of_info().any(|info| info.frozen.contains(&on));
				Ok(ConstantOutput::Value(if is_frozen { TypeId::TRUE } else { TypeId::FALSE }))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"defineProperty" => {
			// TODO check configurable
			if let [on, property, descriptor] = arguments {
				let on = on
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
				let property = property
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
				let descriptor = descriptor
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;

				let under = PropertyKey::from_type(property, types);
				// TODO
				let mut behavior = CheckThings { debug_types: true };
				let publicity = Publicity::Public;
				let mode = AccessMode::Regular;

				// This doesn't allow generic keys, but it starts to get very weird if that is something to be allowed
				macro_rules! get_property {
					($key:expr) => {
						crate::types::properties::get_property(
							descriptor,
							publicity,
							&PropertyKey::String(std::borrow::Cow::Borrowed($key)),
							environment,
							(&mut behavior, diagnostics),
							types,
							call_site,
							mode,
						)
						.map(|(_, value)| value)
					};
				}

				// TODO what about two specified, what about order
				let value = if let Some(value) = get_property!("value") {
					// TODO check no get or set otherwise causes type error
					PropertyValue::Value(value)
				} else {
					let getter = get_property!("get");
					let setter = get_property!("set");
					if let (Some(getter), Some(setter)) = (getter, setter) {
						// This a weird one, the only time is created wholey rather
						// from accumulation in resolver
						PropertyValue::GetterAndSetter {
							getter: Callable::from_type(getter, types),
							setter: Callable::from_type(setter, types),
						}
					} else if let Some(getter) = getter {
						PropertyValue::Getter(Callable::from_type(getter, types))
					} else if let Some(setter) = setter {
						PropertyValue::Setter(Callable::from_type(setter, types))
					} else {
						return Err(ConstantFunctionError::CannotComputeConstant);
					}
				};

				// For configurablity
				let existing = crate::types::properties::get_property_unbound(
					(on, None),
					(Publicity::Public, &under, None),
					false,
					environment,
					types,
				);

				if let Ok(LogicalOrValid::Logical(Logical::Pure(PropertyValue::Configured {
					on,
					descriptor: Descriptor { writable: _, enumerable: _, configurable },
				}))) = existing
				{
					// WIP doesn't cover all valid cases
					if configurable != TypeId::TRUE {
						let valid =
							if let (PropertyValue::Value(existing), PropertyValue::Value(new)) =
								(*on, &value)
							{
								// yah weird spec
								existing == *new
							} else {
								false
							};
						if !valid {
							return Err(ConstantFunctionError::FunctionCallingError(
								FunctionCallingError::NotConfigurable {
									property: crate::diagnostics::PropertyKeyRepresentation::new(
										&under,
										environment,
										types,
									),
									// Should be set by parent
									call_site,
								},
							));
						}
					}
				}

				// FALSE is spec here!
				let writable = get_property!("writable").unwrap_or(TypeId::FALSE);
				let enumerable = get_property!("enumerable").unwrap_or(TypeId::FALSE);
				let configurable = get_property!("configurable").unwrap_or(TypeId::FALSE);

				crate::utilities::notify!(
					"values are = {:?}",
					(writable, enumerable, configurable)
				);

				// Can skip if (read defaults to `true`)
				let value = if let (TypeId::TRUE, TypeId::TRUE, TypeId::TRUE) =
					(writable, enumerable, configurable)
				{
					value
				} else {
					PropertyValue::Configured {
						on: Box::new(value),
						descriptor: Descriptor { writable, enumerable, configurable },
					}
				};

				environment.info.register_property(on, Publicity::Public, under, value, call_site);

				Ok(ConstantOutput::Value(on))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"getOwnPropertyDescriptor" => {
			if let [on, property] = arguments {
				let on = on
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
				let property = property
					.non_spread_type()
					.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;

				let value = crate::types::properties::resolver(
					(on, None),
					(Publicity::Public, &PropertyKey::from_type(property, types), None),
					environment,
					types,
				);
				crate::utilities::notify!("value is = {:?}", value);

				match value {
					Some((value, _, _)) => {
						let mut descriptor = crate::types::properties::Descriptor::default();
						let mut object =
							ObjectBuilder::new(None, types, call_site, &mut environment.info);

						match value.inner_simple() {
							PropertyValue::Value(ty) => {
								object.append(
									Publicity::Public,
									"value".into(),
									PropertyValue::Value(*ty),
									call_site,
									&mut environment.info,
								);
							}
							PropertyValue::GetterAndSetter { getter, setter } => {
								object.append(
									Publicity::Public,
									"get".into(),
									PropertyValue::Value(getter.into_type(types)),
									call_site,
									&mut environment.info,
								);
								object.append(
									Publicity::Public,
									"set".into(),
									PropertyValue::Value(setter.into_type(types)),
									call_site,
									&mut environment.info,
								);
							}
							PropertyValue::Getter(getter) => {
								object.append(
									Publicity::Public,
									"get".into(),
									PropertyValue::Value(getter.into_type(types)),
									call_site,
									&mut environment.info,
								);
							}
							PropertyValue::Setter(setter) => {
								object.append(
									Publicity::Public,
									"set".into(),
									PropertyValue::Value(setter.into_type(types)),
									call_site,
									&mut environment.info,
								);
							}
							PropertyValue::Deleted => {
								return Ok(ConstantOutput::Value(TypeId::UNDEFINED_TYPE));
							}
							_ => unreachable!(),
						}

						match value {
							PropertyValue::ConditionallyExists { .. } => {
								crate::utilities::notify!("TODO conditional. Union with undefined");
								return Err(ConstantFunctionError::CannotComputeConstant);
							}
							PropertyValue::Configured { on: _, descriptor: d } => {
								descriptor = d;
							}
							_ => {}
						}

						object.append(
							Publicity::Public,
							"writable".into(),
							PropertyValue::Value(descriptor.writable),
							call_site,
							&mut environment.info,
						);
						object.append(
							Publicity::Public,
							"enumerable".into(),
							PropertyValue::Value(descriptor.enumerable),
							call_site,
							&mut environment.info,
						);
						object.append(
							Publicity::Public,
							"configurable".into(),
							PropertyValue::Value(descriptor.configurable),
							call_site,
							&mut environment.info,
						);

						Ok(ConstantOutput::Value(object.build_object()))
					}
					// IMO this should be `TypeId::NULL` :(
					None => Ok(ConstantOutput::Value(TypeId::UNDEFINED_TYPE)),
				}
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		"proxy:constructor" => {
			crate::utilities::notify!("Here creating proxy");
			if let [object, trap] = arguments {
				// TODO checking for both, what about spreading
				let value = types.register_type(Type::SpecialObject(
					crate::features::objects::SpecialObject::Proxy(Proxy {
						handler: trap.non_spread_type().expect("single type"),
						over: object.non_spread_type().expect("single type"),
					}),
				));
				Ok(ConstantOutput::Value(value))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		// "RegExp:constructor" => {
		// 	crate::utilities::notify!("TODO check argument");
		// 	if let Some(arg) = arguments.first() {
		// 		Ok(ConstantOutput::Value(features::regular_expressions::new_regexp(features::regular_expressions::TypeIdOrString::TypeId(arg), types, environment)))
		// 	} else {
		// 		Err(ConstantFunctionError::CannotComputeConstant)
		// 	}
		// }
		// TODO
		"JSON:parse" => {
			crate::utilities::notify!("TODO JSON:parse");
			Err(ConstantFunctionError::CannotComputeConstant)
		}
		"JSON:stringify" => {
			crate::utilities::notify!("TODO JSON:stringify");
			Err(ConstantFunctionError::CannotComputeConstant)
		}
		"regexp:constructor" => {
			let pattern = types
				.get_type_by_id(arguments.first().unwrap().non_spread_type().expect("pattern"));
			let flags =
				arguments.get(1).map(|a| types.get_type_by_id(a.non_spread_type().expect("flags")));

			let Type::Constant(Constant::String(pattern)) = pattern else {
				return Err(ConstantFunctionError::CannotComputeConstant);
			};
			let flags = match flags {
				Some(flags) => {
					let Type::Constant(Constant::String(flags)) = flags else {
						return Err(ConstantFunctionError::CannotComputeConstant);
					};

					Some(flags.clone())
				}
				None => None,
			};

			let regexp = types.new_regexp(&pattern.clone(), &flags, &call_site.without_source());

			match regexp {
				Ok(regex) => Ok(ConstantOutput::Value(regex)),
				Err(error) => Err(ConstantFunctionError::FunctionCallingError(
					FunctionCallingError::InvalidRegExp(crate::diagnostics::InvalidRegExp {
						error,
						position: call_site,
					}),
				)),
			}
		}
		"regexp:exec" => {
			let this = this_argument.get_passed().map(|t| types.get_type_by_id(t));

			if let Some(Type::SpecialObject(SpecialObject::RegularExpression(regexp))) = this {
				let pattern_type_id =
					arguments.first().unwrap().non_spread_type().expect("pattern");

				Ok(ConstantOutput::Value(regexp.clone().exec(
					pattern_type_id,
					types,
					environment,
					call_site,
				)))
			} else {
				Err(ConstantFunctionError::CannotComputeConstant)
			}
		}
		// "satisfies" => {
		// 	let ty = arguments
		// 		.first()
		// 		.ok_or(ConstantFunctionError::CannotComputeConstant)?
		// 		.non_spread_type()
		// 		.map_err(|()| ConstantFunctionError::CannotComputeConstant)?;
		// 	// TODO temp!!!
		// 	let arg = call_site_type_args
		// 		.iter()
		// 		.flatten()
		// 		.next()
		// 		.ok_or(ConstantFunctionError::CannotComputeConstant)?
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
		"debug_state" => {
			Ok(ConstantOutput::Diagnostic(format!("State={:?}", environment.info.state)))
		}
		"debug_context" => Ok(ConstantOutput::Diagnostic(environment.debug())),
		"context_id" => Ok(ConstantOutput::Diagnostic(format!("in {:?}", environment.context_id))),
		"context_id_chain" => Ok(ConstantOutput::Diagnostic({
			use std::fmt::Write;
			let mut buf = format!("{:?}", environment.context_id);
			for ctx in environment.parents_iter().skip(1) {
				write!(&mut buf, " <- {:?}", get_on_ctx!(ctx.context_id))
					.map_err(|_| ConstantFunctionError::CannotComputeConstant)?;
			}
			buf
		})),
		"is_dependent" => Ok(ConstantOutput::Diagnostic(format!(
			"is dependent {:?}",
			types
				.get_type_by_id(
					arguments
						.first()
						.ok_or(ConstantFunctionError::CannotComputeConstant)?
						.non_spread_type()
						.map_err(|()| ConstantFunctionError::CannotComputeConstant)?
				)
				.is_dependent()
		))),
		// "compile_type_to_object" => {
		// 	if let Some(value) = call_site_type_args {
		// 		let value = crate::types::others::create_object_for_type(
		// 			value.first().ok_or(ConstantFunctionError::CannotComputeConstant)?.0,
		// 			environment,
		// 			types,
		// 			call_site,
		// 		);
		// 		Ok(ConstantOutput::Value(value))
		// 	} else {
		// 		Err(ConstantFunctionError::CannotComputeConstant)
		// 	}
		// }
		func => {
			// Sometimes a bad definition file could occur so...
			Err(ConstantFunctionError::NoLogicForIdentifier(func.to_owned()))
		}
	}
}

use iterator_endiate::EndiateIteratorExt;
use std::collections::HashSet;

use super::{GenericChain, PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::information::{get_value_of_constant_import_variable, InformationChain},
	features::objects::{Proxy, SpecialObject},
	types::{
		calling::ThisValue,
		functions::{FunctionBehavior, FunctionEffect},
		generics::generic_type_arguments::GenericArguments,
		get_constraint, get_simple_property_value,
		helpers::get_array_length,
		properties::{get_properties_on_single_type, AccessMode, PropertyKey, Publicity},
		Constructor, GenericChainLink, ObjectNature, PartiallyAppliedGenerics, TypeExtends,
	},
	PropertyValue,
};

#[must_use]
pub fn print_type(
	id: TypeId,
	types: &TypeStore,
	info_chain: &impl InformationChain,
	debug: bool,
) -> String {
	let mut buf = String::new();
	print_type_into_buf(
		id,
		&mut buf,
		&mut HashSet::new(),
		GenericChain::None,
		types,
		info_chain,
		debug,
	);
	buf
}

#[must_use]
pub fn print_type_with_type_arguments(
	id: TypeId,
	type_arguments: GenericChain,
	types: &TypeStore,
	info_chain: &impl InformationChain,
	debug: bool,
) -> String {
	let mut buf = String::new();
	print_type_into_buf(
		id,
		&mut buf,
		&mut HashSet::new(),
		type_arguments,
		types,
		info_chain,
		debug,
	);
	buf
}

/// Recursion safe + reuses buffer
pub fn print_type_into_buf<C: InformationChain>(
	ty: TypeId,
	buf: &mut String,
	cycles: &mut HashSet<TypeId>,
	args: GenericChain,
	types: &TypeStore,
	info: &C,
	debug: bool,
) {
	use std::fmt::Write;

	let not_in_cycle = cycles.insert(ty);
	if !not_in_cycle {
		buf.push_str("*cycle*");
		return;
	}

	let r#type = types.get_type_by_id(ty);
	match r#type {
		Type::And(a, b) => {
			// let value = crate::types::intrinsics::get_range_and_mod_class(ty, types);
			// if value.0.is_some() || value.1.is_some() {
			// 	crate::utilities::notify!("{:?}", value);
			// }

			print_type_into_buf(*a, buf, cycles, args, types, info, debug);
			buf.push_str(" & ");
			print_type_into_buf(*b, buf, cycles, args, types, info, debug);
		}
		Type::Or(a, b) => {
			// let value = crate::types::intrinsics::get_range_and_mod_class(ty, types);
			// if value.0.is_some() || value.1.is_some() {
			// 	crate::utilities::notify!("{:?}", value);
			// }

			print_type_into_buf(*a, buf, cycles, args, types, info, debug);
			buf.push_str(" | ");
			print_type_into_buf(*b, buf, cycles, args, types, info, debug);
		}
		Type::Narrowed { narrowed_to, from } => {
			if debug {
				write!(buf, "(narrowed from {from:?}) ").unwrap();
			}
			print_type_into_buf(*narrowed_to, buf, cycles, args, types, info, debug);
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::MappedGeneric { name, extends } => {
				if debug {
					write!(buf, "(mg {} {}) ", ty.0, name).unwrap();
				}
				crate::utilities::notify!("args={:?}", args);
				if let Some(crate::types::CovariantContribution::String(property)) =
					args.and_then(|arg| arg.get_argument_covariant(ty))
				{
					write!(buf, "{property}").unwrap();
				} else {
					print_type_into_buf(*extends, buf, cycles, args, types, info, debug);
				}
			}
			PolyNature::FunctionGeneric { name, .. }
			| PolyNature::StructureGeneric { name, extends: _ } => {
				if debug {
					if let PolyNature::FunctionGeneric { .. } = nature {
						write!(buf, "(fg {} {})", ty.0, name).unwrap();
					} else {
						write!(buf, "(sg {} {})", ty.0, name).unwrap();
					}
				} else if let Some(arg) = args.and_then(|args| args.get_argument_covariant(ty)) {
					use crate::types::CovariantContribution;
					if debug {
						buf.push_str("(specialised with) ");
					}
					match arg {
						CovariantContribution::TypeId(id) => {
							print_type_into_buf(id, buf, cycles, args, types, info, debug);
						}
						arg => {
							crate::utilities::notify!("TODO print {:?}", arg);
						}
					}

					// for (more, arg) in structure_args.iter().nendiate() {
					// 	print_type_into_buf(*arg, buf, cycles, args, types, info, debug);
					// 	if more {
					// 		buf.push_str(" | ");
					// 	}
					// }
				} else {
					buf.push_str(name);
				}
			}
			PolyNature::InferGeneric { name, extends } => {
				if debug {
					write!(buf, "(IG {}) @ ", ty.0).unwrap();
				}
				buf.push_str("infer ");
				buf.push_str(name);
				if *extends != TypeId::ANY_TYPE {
					buf.push_str(" extends ");
					print_type_into_buf(*extends, buf, cycles, args, types, info, debug);
				}
			}
			PolyNature::FreeVariable { based_on: to, .. } => {
				if debug {
					// FV = free variable
					write!(buf, "(FV {}) @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Parameter { fixed_to: to, variable_id: _ } => {
				if debug {
					write!(buf, "(param {}) @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Open(to) => {
				if debug {
					write!(buf, "(open {}) ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Error(to) => {
				if debug {
					buf.push_str("(error) ");
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::RecursiveFunction(..) => {
				todo!()
				// let modified_base = match env {
				// 	GeneralContext::Syntax(syn) => {
				// 		syn.parents_iter().find_map(|env| get_on_ctx!(env.bases.get(&id)).copied())
				// 	}
				// 	GeneralContext::Root(root) => root.bases.get(&id).copied(),
				// };
			}
			PolyNature::CatchVariable(constraint) => {
				if debug {
					write!(buf, "(CV {ty:?}) ").unwrap();
				}
				print_type_into_buf(*constraint, buf, cycles, args, types, info, debug);
			}
		},
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
			// if let TypeId::Great = *on {
			// 	// let inclusive = *on == TypeId::INCLUSIVE_RANGE;
			// 	let floor =
			// 		arguments.get_structure_restriction(TypeId::NUMBER_GENERIC).unwrap();
			// 	let ceiling =
			// 		arguments.get_structure_restriction(TypeId::NUMBER_CEILING_GENERIC).unwrap();
			// 	if let TypeId::NEG_INFINITY = floor {
			// 		buf.push_str("LessThan<");
			// 		print_type_into_buf(ceiling, buf, cycles, args, types, info, debug);
			// 		buf.push('>');
			// 		return;
			// 	} else if let TypeId::INFINITY = ceiling {
			// 		buf.push_str("GreaterThan<");
			// 		print_type_into_buf(floor, buf, cycles, args, types, info, debug);
			// 		buf.push('>');
			// 		return;
			// 	}
			// }

			if debug {
				write!(buf, "SG({:?})(", ty.0).unwrap();
				print_type_into_buf(*on, buf, cycles, args, types, info, debug);
				buf.push(')');
				match arguments {
					GenericArguments::ExplicitRestrictions(type_restrictions) => {
						buf.push('<');
						let nendiate = type_restrictions.iter().nendiate();
						for (not_at_end, (from, (arg, _))) in nendiate {
							print_type_into_buf(*from, buf, cycles, args, types, info, debug);
							buf.push_str(" = ");
							print_type_into_buf(*arg, buf, cycles, args, types, info, debug);
							if not_at_end {
								buf.push_str(", ");
							}
						}
						buf.push('>');
					}
					GenericArguments::Closure(closures) => {
						write!(buf, "<Closures {closures:?}>").unwrap();
					}
					GenericArguments::LookUp { on } => {
						write!(buf, "<LookUp {on:?}>").unwrap();
					}
				}
			} else if let Type::Class { .. } | Type::Interface { .. } | Type::AliasTo { .. } =
				types.get_type_by_id(*on)
			{
				if debug {
					write!(buf, "SG over ({:?})", types.get_type_by_id(*on)).unwrap();
				}

				// on can be sometimes be generic
				print_type_into_buf(*on, buf, cycles, args, types, info, debug);
				match arguments {
					GenericArguments::ExplicitRestrictions(type_restrictions) => {
						buf.push('<');
						let nendiate = type_restrictions.values().nendiate();
						for (not_at_end, (arg, _)) in nendiate {
							print_type_into_buf(*arg, buf, cycles, args, types, info, debug);
							if not_at_end {
								buf.push_str(", ");
							}
						}
						buf.push('>');
					}
					GenericArguments::Closure(..) | GenericArguments::LookUp { .. } => {}
				}
			} else {
				if debug {
					write!(buf, "SG over ({:?})", types.get_type_by_id(*on)).unwrap();
				}

				let new_arguments = arguments.clone();
				let args = GenericChainLink::append(ty, args.as_ref(), &new_arguments);
				print_type_into_buf(*on, buf, cycles, args, types, info, debug);
			}
		}
		// TODO these can vary
		Type::Constructor(constructor) => match constructor {
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: _,
			} => {
				if let (TypeId::NEVER_TYPE, Ok(crate::types::TypeExtends { item, extends })) =
					(*otherwise_result, crate::types::TypeExtends::from_type(*condition, types))
				{
					buf.push_str("asserts ");
					if let Some(name) = crate::types::helpers::get_reference_name(types, item) {
						buf.push_str(name);
					} else {
						print_type_into_buf(item, buf, cycles, args, types, info, debug);
					}
					buf.push_str(" is ");
					print_type_into_buf(extends, buf, cycles, args, types, info, debug);
					return;
				}

				// TODO nested on constructor
				let is_standard_generic = matches!(
					types.get_type_by_id(*condition),
					Type::RootPolyType(
						PolyNature::InferGeneric { .. }
							| PolyNature::MappedGeneric { .. }
							| PolyNature::FunctionGeneric { .. }
							| PolyNature::StructureGeneric { .. }
					)
				);

				// WIP!
				{
					if let Some(narrowed_value) = info.get_narrowed(*condition) {
						if let crate::Decidable::Known(condition) =
							crate::types::is_type_truthy_falsy(narrowed_value, types)
						{
							let value = if condition { truthy_result } else { otherwise_result };
							print_type_into_buf(*value, buf, cycles, args, types, info, debug);
							return;
						}
					}
				}

				if debug || is_standard_generic {
					if debug {
						write!(buf, "?#{} ", ty.0).unwrap();
					}
					print_type_into_buf(*condition, buf, cycles, args, types, info, debug);
					buf.push_str(" ? ");
				}
				print_type_into_buf(*truthy_result, buf, cycles, args, types, info, debug);
				buf.push_str(if debug || is_standard_generic { " : " } else { " | " });
				print_type_into_buf(*otherwise_result, buf, cycles, args, types, info, debug);
			}
			Constructor::KeyOf(on) => {
				// if get_constraint(*on, types).is_some() {
				// TODO try get and |
				buf.push_str("keyof ");
				print_type_into_buf(*on, buf, cycles, args, types, info, debug);
				// } else {
				// 	let properties = get_properties_on_type(*on, types);
				// 	if properties.is_empty() {
				// 		buf.push_str("never");
				// 	} else {
				// 		let mut iter = properties.into_iter();
				// 		let (_, key, _) = iter.next().unwrap();
				// 		print_property_key_into_buf(
				// 			&key, buf, cycles, args, types, info, debug,
				// 		);
				// 		for (_, key, _) in iter {
				// 			buf.push_str(" | ");
				// 			print_property_key_into_buf(
				// 				&key, buf, cycles, args, types, info, debug,
				// 			);
				// 		}
				// 	}
				// }
			}
			Constructor::Property { on, under, result, mode } => {
				// crate::utilities::notify!("before {:?}", types.get_type_by_id(*on));
				let on = if let Some(crate::types::CovariantContribution::TypeId(value)) =
					args.and_then(|arg| arg.get_argument_covariant(*on))
				{
					value
				} else {
					*on
				};

				// crate::utilities::notify!("after {:?}", types.get_type_by_id(on));

				let is_explicit_generic = crate::types::is_explicit_generic(on, types);

				if let (AccessMode::FromTypeAnnotation, false) = (mode, is_explicit_generic) {
					// TODO args on both?
					let result = crate::types::get_property_unbound(
						(on, args),
						(Publicity::Public, under, args),
						false,
						info,
						types,
					);
					crate::utilities::notify!("result={:?}", result);
					if let Ok(prop) = result {
						match prop {
							crate::types::logical::LogicalOrValid::Logical(
								crate::types::logical::Logical::Pure(value),
							) => {
								let result = value.as_get_type(types);
								print_type_into_buf(result, buf, cycles, args, types, info, debug);
								return;
							}
							value => {
								crate::utilities::notify!("value={:?}", value);
							}
						}
					} else {
						crate::utilities::notify!("Error: no index on type annotation");
					}
				}

				if is_explicit_generic || matches!(mode, AccessMode::FromTypeAnnotation) {
					print_type_into_buf(on, buf, cycles, args, types, info, debug);
					buf.push('[');
					match under {
						PropertyKey::String(s) => {
							buf.push('"');
							buf.push_str(s);
							buf.push('"');
						}
						PropertyKey::Type(t) => {
							print_type_into_buf(*t, buf, cycles, args, types, info, debug);
						}
					};
					buf.push(']');
				} else if let Some(Type::PartiallyAppliedGenerics(sgs)) =
					get_constraint(on, types).map(|ty| types.get_type_by_id(ty))
				{
					let new_arguments = sgs.arguments.clone();
					let args = GenericChainLink::append(ty, args.as_ref(), &new_arguments);
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				} else {
					if debug {
						buf.push_str("(property on ");
						print_type_into_buf(on, buf, cycles, args, types, info, debug);
						buf.push_str(" under ");
						match under {
							PropertyKey::String(s) => {
								buf.push('"');
								buf.push_str(s);
								buf.push('"');
							}
							PropertyKey::Type(t) => {
								print_type_into_buf(*t, buf, cycles, args, types, info, debug);
							}
						}
						write!(buf, "{ty:?}").unwrap();
						buf.push_str(") ");
					}

					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				}
			}
			Constructor::BinaryOperator {
				lhs: _,
				operator: crate::features::operations::MathematicalOrBitwiseOperation::Remainder,
				rhs,
				result: _,
			} if matches!(types.get_type_by_id(*rhs), Type::Constant(_)) => {
				if let Type::Constant(crate::Constant::Number(num)) = types.get_type_by_id(*rhs) {
					write!(buf, "ExclusiveRange<-{num}, {num}>").unwrap();
				} else {
					unreachable!()
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs, result: _ } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info, debug);
					write!(buf, " ({ty:?}){operator:?} ").unwrap();
					print_type_into_buf(*rhs, buf, cycles, args, types, info, debug);
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info, debug);
					let operator = match operator {
						crate::features::operations::CanonicalEqualityAndInequality::StrictEqual => {
							"==="
						}
						crate::features::operations::CanonicalEqualityAndInequality::LessThan => {
							"<"
						}
					};
					write!(buf, " ({ty}){operator} ", ty = ty.0).unwrap();
					print_type_into_buf(*rhs, buf, cycles, args, types, info, debug);
				}
				Constructor::TypeOperator(to) => {
					write!(buf, "TypeOperator.{to:?}").unwrap();
				}
				Constructor::TypeExtends(TypeExtends { item, extends }) => {
					if let Some(name) = crate::types::helpers::get_reference_name(types, *item) {
						buf.push_str(name);
					} else {
						print_type_into_buf(*item, buf, cycles, args, types, info, debug);
					}
					buf.push_str(" extends ");
					print_type_into_buf(*extends, buf, cycles, args, types, info, debug);
				}
				Constructor::Image { on: _, with: _, result } => {
					// TODO arguments
					write!(buf, "(func result {}) (*args*)", ty.0).unwrap();
					buf.push_str(" -> ");
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				}
				Constructor::Property { on, under, result, mode } => {
					buf.push('(');
					print_type_into_buf(*on, buf, cycles, args, types, info, debug);
					buf.push_str(")[");
					print_property_key_into_buf(under, buf, cycles, args, types, info, debug);
					buf.push(']');
					if let AccessMode::DoNotBindThis = mode {
						buf.push_str(" no binding of `this`");
					};
					buf.push_str(" = ");
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				}
				Constructor::Awaited { on, result } => {
					if debug {
						buf.push_str("Awaited<");
						print_type_into_buf(*on, buf, cycles, args, types, info, debug);
						buf.push_str(", R=");
						print_type_into_buf(*result, buf, cycles, args, types, info, debug);
						buf.push('>');
					} else {
						print_type_into_buf(*result, buf, cycles, args, types, info, debug);
					}
				}
				Constructor::ConditionalResult { .. } | Constructor::KeyOf(..) => {
					unreachable!()
				}
			},
			constructor => {
				if let Constructor::BinaryOperator { result: TypeId::STRING_TYPE, .. } = constructor
				{
					let slice =
						crate::types::helpers::TemplatelLiteralExpansion::from_type(ty, types);
					if let Some(single) = slice.as_single_string() {
						buf.push('"');
						buf.push_str(single);
						buf.push('"');
					} else {
						buf.push('`');
						for (s, ty) in &slice.parts {
							buf.push_str(s);
							buf.push_str("${");
							print_type_into_buf(*ty, buf, cycles, args, types, info, debug);
							buf.push('}');
						}
						buf.push_str(&slice.rest);
						buf.push('`');
					}
					return;
				}
				let base = get_constraint(ty, types).unwrap();
				print_type_into_buf(base, buf, cycles, args, types, info, debug);
			}
		},
		t @ (Type::Class { name, type_parameters: _, .. }
		| Type::Interface { name, parameters: _, .. }
		| Type::AliasTo { to: _, name, parameters: _ }) => {
			if debug {
				write!(buf, "{name}#{}", ty.0).unwrap();
				if let Type::AliasTo { to, .. } = t {
					buf.push_str(" = ");
					print_type_into_buf(*to, buf, cycles, args, types, info, debug);
				} else if let Type::Class { .. } = t {
					buf.push_str(" (class)");
				}
			} else {
				buf.push_str(name);
			}

			// TODO under option
			// if let Type::AliasTo { to, .. } = t {
			// 	buf.push_str(" = ");
			// 	print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			// }

			// TODO only if not partially applied generic
			// if let (true, Some(parameters)) = (debug, parameters) {
			// 	buf.push('{');
			// 	for param in parameters {
			// 		print_type_into_buf(*param, buf, cycles, args, types, info_chain, debug);
			// 		buf.push_str(", ");
			// 	}
			// 	buf.push('}');
			// }
		}
		Type::Constant(cst) => {
			if debug {
				write!(buf, "({}) {}", ty.0, cst.as_type_name()).unwrap();
			} else {
				buf.push_str(&cst.as_type_name());
			}
		}
		Type::FunctionReference(func_id)
		| Type::SpecialObject(SpecialObject::Function(func_id, _)) => {
			let func = types.functions.get(func_id).unwrap();

			if let FunctionBehavior::Constructor { ref name, prototype, .. } = func.behavior {
				if let Type::Constant(crate::Constant::String(name)) = types.get_type_by_id(*name) {
					if debug {
						write!(buf, "constructor(for#{})@{name}#{}", prototype.0, ty.0).unwrap();
					} else {
						buf.push_str(name);
						return;
					}
				} else {
					buf.push_str("*class*");
				}
			}

			if debug {
				let kind = if matches!(r#type, Type::FunctionReference(_)) { "ref" } else { "" };
				write!(buf, "(func{kind} #{}, kind {:?}, effect ", ty.0, func.behavior).unwrap();
				if let FunctionEffect::SideEffects {
					events: _,
					free_variables,
					closed_over_variables,
				} = &func.effect
				{
					write!(buf, "*side effects* {free_variables:?} {closed_over_variables:?}")
						.unwrap();
				} else {
					write!(buf, "{:?}", func.effect).unwrap();
				}
				if let Type::SpecialObject(SpecialObject::Function(_, ThisValue::Passed(p))) =
					r#type
				{
					buf.push_str(", this ");
					print_type_into_buf(*p, buf, cycles, args, types, info, debug);
				}
				buf.push_str(") = ");
			}
			if let Some(ref parameters) = func.type_parameters {
				buf.push('<');
				for (not_at_end, param) in parameters.0.iter().nendiate() {
					buf.push_str(&param.name);
					// if param.constraint != TypeId::ANY_TYPE {
					// 	// TODO `extends keyof` visual compat
					// 	buf.push_str(" extends ");
					// 	// let ty = memory.get_fixed_constraint(constraint);
					// 	// TypeDisplay::fmt(ty, buf, indent, cycles, memory);
					// }
					if let Some(ref _default) = param.default {
						todo!()
					}
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push('>');
			}
			// TODO don't think this is needed
			// let args = if let Type::SpecialObject(SpecialObjects::Function(_, this)) = r#type {
			// 	if let Some(Type::PartiallyAppliedGenerics(sgs))) = this
			// 		.get_passed()
			// 		.map(|ty| get_constraint(ty, types).unwrap_or(ty))
			// 		.map(|ty| types.get_type_by_id(ty))
			// 	{
			// 		Some(GenericChain::append(args.as_ref(), &sgs.arguments))
			// 	} else {
			// 		args
			// 	}
			// } else {
			// 	args
			// };
			buf.push('(');
			for (not_at_end, param) in func.parameters.parameters.iter().nendiate() {
				buf.push_str(&param.name);
				buf.push_str(": ");
				print_type_into_buf(param.ty, buf, cycles, args, types, info, debug);
				if func.parameters.rest_parameter.is_some() || not_at_end {
					buf.push_str(", ");
				}
			}
			if let Some(ref rest_parameter) = func.parameters.rest_parameter {
				buf.push_str("...");
				print_type_into_buf(rest_parameter.ty, buf, cycles, args, types, info, debug);
			}
			buf.push_str(") => ");
			print_type_into_buf(func.return_type, buf, cycles, args, types, info, debug);
		}
		Type::Object(kind) => {
			if debug {
				if let ObjectNature::RealDeal = kind {
					write!(buf, "(obj {}) ", ty.0).unwrap();
				} else {
					write!(buf, "(anom {}) ", ty.0).unwrap();
				}
			}
			let prototype =
				info.get_chain_of_info().find_map(|info| info.prototypes.get(&ty).copied());

			if let Some(TypeId::ARRAY_TYPE) = prototype {
				match get_array_length(info, ty, types) {
					Ok(n) => {
						buf.push('[');
						for i in 0..(n as usize) {
							if i != 0 {
								buf.push_str(", ");
							}
							let value = get_simple_property_value(
								info,
								ty,
								&PropertyKey::from_usize(i),
								types,
							);

							if let Some(value) = value {
								print_type_into_buf(value, buf, cycles, args, types, info, debug);
							} else {
								// TODO sometimes the above is not always `None` as `None` can occur for complex keys...
								buf.push_str("*empty*");
							}
						}
						buf.push(']');
					}
					Err(n) => {
						if let Some(n) = n {
							crate::utilities::notify!("Printing array with length={:?}", n);
						} else {
							crate::utilities::notify!("Printing array with no length");
						}

						if debug {
							let properties = get_properties_on_single_type(
								ty,
								types,
								info,
								false,
								TypeId::ANY_TYPE,
							);
							crate::utilities::notify!("Array {:?}", properties);
						}
						// TODO get property
						write!(buf, "Array<*things*>").unwrap();
					}
				}
			} else {
				if let Some(prototype) =
					prototype.filter(|prototype| !matches!(*prototype, TypeId::NULL_TYPE))
				{
					// crate::utilities::notify!("P during print {:?}", prototype);
					buf.push('[');
					print_type_into_buf(prototype, buf, cycles, args, types, info, debug);
					buf.push_str("] ");
				} else {
					// crate::utilities::notify!("no P on {:?} during print", id);
				}
				// Important!
				let filter_enumerable = false;
				let properties = get_properties_on_single_type(
					ty,
					types,
					info,
					filter_enumerable,
					TypeId::ANY_TYPE,
				);
				if properties.is_empty() {
					buf.push_str("{}");
					return;
				}

				buf.push_str("{ ");
				for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
					if let Publicity::Private = publicity {
						buf.push('#');
					}

					// let root;
					// let args = if let Some((id, to)) = key.mapped_generic_id(types) {
					// 	let mut map = crate::Map::default();
					// 	map.insert(id, (to, SpanWithSource::NULL));
					// 	root = GenericArguments::ExplicitRestrictions(map);
					// 	Some(GenericChainLink::append_to_link(id, args.as_ref(), &root))
					// } else {
					// 	args
					// };

					let is_optional = value.is_optional_simple();
					let is_readonly = value.is_writable_simple();

					if is_readonly {
						buf.push_str("readonly ");
					}

					// TODO methods here

					match value.inner_simple() {
						PropertyValue::Value(value) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(if is_optional { "?: " } else { ": " });
							print_type_into_buf(*value, buf, cycles, args, types, info, debug);
						}
						PropertyValue::Getter(getter) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(if is_optional { "?: " } else { ": " });
							buf.push_str("(getter) ");
							print_type_into_buf(
								getter.get_return_type(types),
								buf,
								cycles,
								args,
								types,
								info,
								debug,
							);
						}
						PropertyValue::GetterAndSetter { getter, setter } => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(if is_optional { "?: " } else { ": " });
							buf.push_str("(getter) ");
							print_type_into_buf(
								getter.get_return_type(types),
								buf,
								cycles,
								args,
								types,
								info,
								debug,
							);
							buf.push_str(" + (setter) ");
							print_type_into_buf(
								setter.get_first_argument(types),
								buf,
								cycles,
								args,
								types,
								info,
								debug,
							);
						}
						PropertyValue::Setter(setter) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(if is_optional { "?: " } else { ": " });
							buf.push_str("(setter) ");
							print_type_into_buf(
								setter.get_first_argument(types),
								buf,
								cycles,
								args,
								types,
								info,
								debug,
							);
						}
						PropertyValue::Deleted => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(if is_optional { "?: " } else { ": " });
							buf.push_str("never");
						}
						PropertyValue::ConditionallyExists { .. }
						| PropertyValue::Configured { .. } => {
							unreachable!("should be unreachable by `inner_simple`")
						}
					}

					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
		}
		Type::SpecialObject(special_object) => match special_object {
			SpecialObject::Null => {
				buf.push_str("null");
			}
			SpecialObject::Promise { .. } => todo!(),
			SpecialObject::Generator { .. } => todo!(),
			SpecialObject::Proxy(Proxy { handler, over }) => {
				// Copies from node behavior
				buf.push_str("Proxy [ ");
				print_type_into_buf(*over, buf, cycles, args, types, info, debug);
				buf.push_str(", ");
				print_type_into_buf(*handler, buf, cycles, args, types, info, debug);
				buf.push_str(" ]");
			}
			SpecialObject::Import(exports) => {
				buf.push_str("{ ");
				for (not_at_end, (key, (variable, mutability))) in exports.named.iter().nendiate() {
					buf.push_str(key);
					buf.push_str(": ");
					match mutability {
						crate::features::variables::VariableMutability::Constant => {
							let value = get_value_of_constant_import_variable(*variable, info);
							print_type_into_buf(value, buf, cycles, args, types, info, debug);
						}
						crate::features::variables::VariableMutability::Mutable {
							reassignment_constraint: _,
						} => todo!(),
					}
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
			SpecialObject::RegularExpression(exp) => {
				buf.push_str(exp.source());
			}
			SpecialObject::Function(..) => unreachable!(),
		},
	}

	cycles.remove(&ty);
}

#[must_use]
pub fn print_property_key<C: InformationChain>(
	key: &PropertyKey,
	types: &TypeStore,
	info: &C,
	debug: bool,
) -> String {
	let mut string = String::new();
	print_property_key_into_buf(
		key,
		&mut string,
		&mut HashSet::new(),
		GenericChain::None,
		types,
		info,
		debug,
	);
	string
}

pub(crate) fn print_property_key_into_buf<C: InformationChain>(
	key: &PropertyKey,
	buf: &mut String,
	cycles: &mut HashSet<TypeId>,
	args: GenericChain,
	types: &TypeStore,
	info: &C,
	debug: bool,
) {
	match key {
		PropertyKey::String(s) => buf.push_str(s),
		PropertyKey::Type(t) => {
			buf.push('[');
			print_type_into_buf(*t, buf, cycles, args, types, info, debug);
			buf.push(']');
		}
	}
}

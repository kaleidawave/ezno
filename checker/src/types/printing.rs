use iterator_endiate::EndiateIteratorExt;
use std::collections::HashSet;

use super::{properties::PropertyKey, GenericChain, PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::{
		information::{
			self, get_properties_on_type, get_value_of_constant_import_variable, InformationChain,
			Publicity,
		},
		Logical,
	},
	events::{Event, FinalEvent},
	features::{functions::ThisValue, objects::SpecialObjects},
	types::{
		get_constraint, generics::generic_type_arguments::StructureGenericArguments, Constructor,
		FunctionEffect, GenericChainLink, ObjectNature, StructureGenerics, TypeRelationOperator,
	},
	Constant, PropertyValue,
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
fn print_type_into_buf<C: InformationChain>(
	ty: TypeId,
	buf: &mut String,
	cycles: &mut HashSet<TypeId>,
	args: GenericChain,
	types: &TypeStore,
	info_chain: &C,
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
			print_type_into_buf(*a, buf, cycles, args, types, info_chain, debug);
			buf.push_str(" & ");
			print_type_into_buf(*b, buf, cycles, args, types, info_chain, debug);
		}
		Type::Or(a, b) => {
			print_type_into_buf(*a, buf, cycles, args, types, info_chain, debug);
			buf.push_str(" | ");
			print_type_into_buf(*b, buf, cycles, args, types, info_chain, debug);
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::FunctionGeneric { name, .. }
			| PolyNature::StructureGeneric { name, constrained: _ } => {
				if let Some(structure_args) =
					args.and_then(|args| args.get_argument(ty, info_chain, types))
				{
					for (more, arg) in structure_args.iter().nendiate() {
						print_type_into_buf(*arg, buf, cycles, args, types, info_chain, debug);
						if more {
							buf.push_str(" | ");
						}
					}
				} else if debug {
					if let PolyNature::FunctionGeneric { eager_fixed, .. } = nature {
						write!(buf, "[fg {} {}, @ ", name, ty.0).unwrap();
						print_type_into_buf(
							*eager_fixed,
							buf,
							cycles,
							args,
							types,
							info_chain,
							debug,
						);
						buf.push(']');
					} else {
						write!(buf, "[sg {} {}]", name, ty.0).unwrap();
					}
				} else {
					buf.push_str(name);
				}
			}
			PolyNature::FreeVariable { based_on: to, .. } => {
				if debug {
					// FV = free variable
					write!(buf, "[FV {}] @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info_chain, debug);
			}
			PolyNature::Parameter { fixed_to: to } => {
				if debug {
					write!(buf, "[param {}] @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info_chain, debug);
			}
			PolyNature::Open(to) => {
				if debug {
					write!(buf, "[open {}] ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info_chain, debug);
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
		},
		// TODO these can vary
		Type::Constructor(constructor) => match constructor {
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: _,
			} => {
				if debug {
					write!(buf, "?#{} ", ty.0).unwrap();
					print_type_into_buf(*condition, buf, cycles, args, types, info_chain, debug);
					buf.push_str("? ");
				}
				print_type_into_buf(*truthy_result, buf, cycles, args, types, info_chain, debug);
				buf.push_str(if debug { " : " } else { " | " });
				print_type_into_buf(*otherwise_result, buf, cycles, args, types, info_chain, debug);
			}
			Constructor::StructureGenerics(StructureGenerics { on, arguments }) => {
				if debug {
					write!(buf, "SG({:?})(", ty.0).unwrap();
					print_type_into_buf(*on, buf, cycles, args, types, info_chain, debug);
					buf.push(')');
					match arguments {
						StructureGenericArguments::ExplicitRestrictions(type_restrictions) => {
							buf.push('<');
							let nendiate = type_restrictions.iter().nendiate();
							for (not_at_end, (from, (arg, _))) in nendiate {
								print_type_into_buf(
									*from, buf, cycles, args, types, info_chain, debug,
								);
								buf.push_str(" = ");
								print_type_into_buf(
									*arg, buf, cycles, args, types, info_chain, debug,
								);
								if not_at_end {
									buf.push_str(", ");
								}
							}
							buf.push('>');
						},
						StructureGenericArguments::Closure(closures) => {
							write!(buf, "<Closures {closures:?}>").unwrap();
						}
						StructureGenericArguments::LookUp { on } => {
							write!(buf, "<Lookup {on:?}>").unwrap();
						}
					}
				} else if let Type::Class { .. } | Type::Interface { .. } | Type::AliasTo { .. } =
					types.get_type_by_id(*on)
				{
					// on can be sometimes be generic
					print_type_into_buf(*on, buf, cycles, args, types, info_chain, debug);
					match arguments {
						StructureGenericArguments::ExplicitRestrictions(type_restrictions) => {
							buf.push('<');
							let nendiate = type_restrictions.values().nendiate();
							for (not_at_end, (arg, _)) in nendiate {
								print_type_into_buf(
									*arg, buf, cycles, args, types, info_chain, debug,
								);
								if not_at_end {
									buf.push_str(", ");
								}
							}
							buf.push('>');
						}
						StructureGenericArguments::Closure(..)
						| StructureGenericArguments::LookUp { .. } => {}
					}
				} else {
					print_type_into_buf(
						*on,
						buf,
						cycles,
						GenericChainLink::append(args.as_ref(), arguments),
						types,
						info_chain,
						debug,
					);
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info_chain, debug);
					write!(buf, " {operator:?} ").unwrap();
					print_type_into_buf(*rhs, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info_chain, debug);
					match operator {
							crate::features::operations::CanonicalEqualityAndInequality::StrictEqual => {
								buf.push_str(" === ");
							}
							crate::features::operations::CanonicalEqualityAndInequality::LessThan => {
								buf.push_str(" < ");
							}
						}
					print_type_into_buf(*rhs, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::UnaryOperator { operator, operand } => {
					write!(buf, "{operator:?} ").unwrap();
					print_type_into_buf(*operand, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::TypeOperator(to) => {
					write!(buf, "TypeOperator = {to:?}").unwrap();
				}
				Constructor::TypeRelationOperator(TypeRelationOperator::Extends {
					ty,
					extends,
				}) => {
					print_type_into_buf(*ty, buf, cycles, args, types, info_chain, debug);
					buf.push_str(" extends ");
					print_type_into_buf(*extends, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::Image { on: _, with: _, result } => {
					write!(buf, "[func result {}] (*args here*)", ty.0).unwrap();
					// TODO arguments
					buf.push_str(" -> ");
					print_type_into_buf(*result, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::Property { on, under, result, bind_this } => {
					buf.push('(');
					print_type_into_buf(*on, buf, cycles, args, types, info_chain, debug);
					buf.push_str(")[");
					print_property_key_into_buf(under, buf, cycles, args, types, info_chain, debug);
					buf.push(']');
					if !bind_this {
						buf.push_str(" no bind");
					};
					buf.push_str(" = ");
					print_type_into_buf(*result, buf, cycles, args, types, info_chain, debug);
				}
				Constructor::StructureGenerics { .. } | Constructor::ConditionalResult { .. } => {
					unreachable!()
				}
				Constructor::Awaited { on, result } => {
					if debug {
						buf.push_str("Awaited<");
						print_type_into_buf(*on, buf, cycles, args, types, info_chain, debug);
						buf.push_str(", R=");
						print_type_into_buf(*result, buf, cycles, args, types, info_chain, debug);
						buf.push('>');
					} else {
						print_type_into_buf(*result, buf, cycles, args, types, info_chain, debug);
					}
				}
			},
			Constructor::Property { on, under, result, bind_this: _ } => {
				if crate::types::is_explicit_generic(*on, types) {
					print_type_into_buf(*on, buf, cycles, args, types, info_chain, debug);
					buf.push('[');
					match under {
						PropertyKey::String(s) => {
							buf.push('"');
							buf.push_str(s);
							buf.push('"');
						}
						PropertyKey::Type(t) => {
							print_type_into_buf(*t, buf, cycles, args, types, info_chain, debug);
						}
					};
					buf.push(']');
				} else if let Some(Type::Constructor(Constructor::StructureGenerics(sgs))) =
					get_constraint(*on, types).map(|ty| types.get_type_by_id(ty))
				{
					print_type_into_buf(
						*result,
						buf,
						cycles,
						GenericChainLink::append(args.as_ref(), &sgs.arguments),
						types,
						info_chain,
						debug,
					);
				} else {
					print_type_into_buf(*result, buf, cycles, args, types, info_chain, debug);
				}
			}
			_constructor => {
				let base = get_constraint(ty, types).unwrap();
				print_type_into_buf(base, buf, cycles, args, types, info_chain, debug);
			}
		},
		Type::Class { name, parameters: _, .. }
		| Type::Interface { name, parameters: _, .. }
		| Type::AliasTo { to: _, name, parameters: _ } => {
			// if debug && ty.0 as usize > TypeId::INTERNAL_TYPE_COUNT {
			// write!(buf, "(r{} nom={:?}) {name}", ty.0, nominal).unwrap();
			// } else {
			buf.push_str(name);
			// }

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
		| Type::SpecialObject(SpecialObjects::Function(func_id, _)) => {
			let func = types.functions.get(func_id).unwrap();
			if debug {
				let kind = if matches!(r#type, Type::FunctionReference(_)) { "ref" } else { "" };
				write!(buf, "[func{kind} #{}, kind {:?}, effect ", ty.0, func.behavior).unwrap();
				if let FunctionEffect::SideEffects {
					events: _,
					free_variables,
					closed_over_variables,
				} = &func.effect
				{
					write!(buf, "*side effects* {free_variables:?} {closed_over_variables:?} ")
						.unwrap();
				} else {
					write!(buf, "{:?} ", func.effect).unwrap();
				}
				if let Type::SpecialObject(SpecialObjects::Function(_, ThisValue::Passed(p))) =
					r#type
				{
					buf.push_str(", this ");
					print_type_into_buf(*p, buf, cycles, args, types, info_chain, debug);
				}
				buf.push_str("] = ");
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
			// 	if let Some(Type::Constructor(Constructor::StructureGenerics(sgs))) = this
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
				print_type_into_buf(param.ty, buf, cycles, args, types, info_chain, debug);
				if func.parameters.rest_parameter.is_some() || not_at_end {
					buf.push_str(", ");
				}
			}
			if let Some(ref rest_parameter) = func.parameters.rest_parameter {
				buf.push_str("...");
				print_type_into_buf(rest_parameter.ty, buf, cycles, args, types, info_chain, debug);
			}
			buf.push_str(") => ");
			print_type_into_buf(func.return_type, buf, cycles, args, types, info_chain, debug);
		}
		Type::Object(kind) => {
			if debug {
				if let ObjectNature::RealDeal = kind {
					write!(buf, "[obj {}]", ty.0).unwrap();
				} else {
					write!(buf, "[aol {}]", ty.0).unwrap();
				}
			}
			let prototype =
				info_chain.get_chain_of_info().find_map(|info| info.prototypes.get(&ty).copied());

			if let Some(TypeId::ARRAY_TYPE) = prototype {
				if let Some(n) = get_array_length(info_chain, ty, types) {
					buf.push('[');
					for i in 0..(n.into_inner() as usize) {
						if i != 0 {
							buf.push_str(", ");
						}
						let value =
							get_simple_value(info_chain, ty, &PropertyKey::from_usize(i), types);

						if let Some(value) = value {
							print_type_into_buf(value, buf, cycles, args, types, info_chain, debug);
						} else {
							// TODO sometimes the above is not always `None` as `None` can occur for complex keys...
							buf.push_str("*empty*");
						}
					}
					buf.push(']');
				} else {
					// TODO get property
					write!(buf, "Array").unwrap();
				}
			} else {
				if let Some(prototype) = prototype {
					// crate::utils::notify!("P during print {:?}", prototype);
					buf.push('[');
					print_type_into_buf(prototype, buf, cycles, args, types, info_chain, debug);
					buf.push_str("] ");
				} else {
					// crate::utils::notify!("no P on {:?} during print", id);
				}
				buf.push_str("{ ");
				let properties = get_properties_on_type(ty, types, info_chain);
				for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
					if let Publicity::Private = publicity {
						buf.push('#');
					}
					print_property_key_into_buf(&key, buf, cycles, args, types, info_chain, debug);
					buf.push_str(": ");
					print_type_into_buf(value, buf, cycles, args, types, info_chain, debug);
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
		}
		Type::SpecialObject(special_object) => match special_object {
			SpecialObjects::Promise { events: () } => todo!(),
			SpecialObjects::Generator { position: () } => todo!(),
			SpecialObjects::Proxy { handler, over } => {
				// Copies from node behavior
				buf.push_str("Proxy [ ");
				print_type_into_buf(*over, buf, cycles, args, types, info_chain, debug);
				buf.push_str(", ");
				print_type_into_buf(*handler, buf, cycles, args, types, info_chain, debug);
				buf.push_str(" ]");
			}
			SpecialObjects::Import(exports) => {
				buf.push_str("{ ");
				for (not_at_end, (key, (variable, mutability))) in exports.named.iter().nendiate() {
					buf.push_str(key);
					buf.push_str(": ");
					match mutability {
						crate::features::variables::VariableMutability::Constant => {
							let value =
								get_value_of_constant_import_variable(*variable, info_chain);
							print_type_into_buf(value, buf, cycles, args, types, info_chain, debug);
						}
						crate::features::variables::VariableMutability::Mutable {
							reassignment_constraint: _,
						} => todo!(),
					};
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
			SpecialObjects::Regexp(exp) => {
				buf.push('/');
				buf.push_str(exp);
				buf.push('/');
			}
			SpecialObjects::Function(..) => unreachable!(),
			SpecialObjects::ClassConstructor { name, constructor: _ } => {
				buf.push_str(name);
			}
		},
	}

	cycles.remove(&ty);
}

/// For getting `length` and stuff
fn get_simple_value(
	ctx: &impl InformationChain,
	on: TypeId,
	property: &PropertyKey,
	types: &TypeStore,
) -> Option<TypeId> {
	fn get_logical(v: Logical<PropertyValue>) -> Option<TypeId> {
		match v {
			Logical::Pure(PropertyValue::Value(t)) => Some(t),
			Logical::Implies { on, antecedent: _ } => get_logical(*on),
			_ => None,
		}
	}

	information::get_property_unbound(on, Publicity::Public, property, types, ctx)
		.ok()
		.and_then(get_logical)
}

fn get_array_length(
	ctx: &impl InformationChain,
	on: TypeId,
	types: &TypeStore,
) -> Option<ordered_float::NotNan<f64>> {
	let length_property = PropertyKey::String(std::borrow::Cow::Borrowed("length"));
	let id = get_simple_value(ctx, on, &length_property, types)?;
	if let Type::Constant(Constant::Number(n)) = types.get_type_by_id(id) {
		Some(*n)
	} else {
		None
	}
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

pub fn debug_effects<C: InformationChain>(
	buf: &mut String,
	events: &[Event],
	types: &TypeStore,
	info: &C,
	debug: bool,
) {
	use std::fmt::Write;

	let args = GenericChain::None;

	for event in events {
		match event {
			Event::ReadsReference { reference, reflects_dependency, position: _ } => {
				write!(buf, "read '{reference:?}' into {reflects_dependency:?}").unwrap();
			}
			Event::SetsVariable(variable, value, _) => {
				write!(buf, "{variable:?}' =").unwrap();
				print_type_into_buf(*value, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::Getter { on, under, reflects_dependency, publicity: _, position: _ } => {
				buf.push_str("read ");
				print_type_into_buf(*on, buf, &mut HashSet::new(), args, types, info, debug);
				if let PropertyKey::String(_) = under {
					buf.push('.');
				}
				print_property_key_into_buf(
					under,
					buf,
					&mut HashSet::new(),
					args,
					types,
					info,
					debug,
				);
				write!(buf, " into {reflects_dependency:?}").unwrap();
			}
			Event::Setter { on, under, new, initialization, publicity: _, position: _ } => {
				if *initialization {
					write!(buf, "initialise {:?} with ", *on).unwrap();
					if let PropertyValue::Value(new) = new {
						print_type_into_buf(
							*new,
							buf,
							&mut HashSet::new(),
							args,
							types,
							info,
							debug,
						);
					}
				} else {
					print_type_into_buf(*on, buf, &mut HashSet::new(), args, types, info, debug);
					buf.push('[');
					print_property_key_into_buf(
						under,
						buf,
						&mut HashSet::default(),
						args,
						types,
						info,
						debug,
					);
					buf.push_str("] = ");
					if let PropertyValue::Value(new) = new {
						print_type_into_buf(
							*new,
							buf,
							&mut HashSet::new(),
							args,
							types,
							info,
							debug,
						);
					}
				}
			}
			Event::CallsType {
				on,
				with: _,
				reflects_dependency,
				timing,
				called_with_new: _,
				position: _,
			} => {
				buf.push_str("call ");
				print_type_into_buf(*on, buf, &mut HashSet::new(), args, types, info, debug);
				write!(buf, " into {reflects_dependency:?} ",).unwrap();
				buf.push_str(match timing {
					crate::events::CallingTiming::Synchronous => "now",
					crate::events::CallingTiming::QueueTask => "queue",
					crate::events::CallingTiming::AtSomePointManyTimes => "sometime",
				});
				// TODO args
			}
			Event::Conditionally {
				condition,
				true_events: events_if_truthy,
				else_events,
				position: _,
			} => {
				buf.push_str("if ");
				print_type_into_buf(*condition, buf, &mut HashSet::new(), args, types, info, debug);
				buf.push_str(" then ");
				debug_effects(buf, events_if_truthy, types, info, debug);
				if !else_events.is_empty() {
					buf.push_str(" else ");
					debug_effects(buf, else_events, types, info, debug);
				}
			}

			Event::CreateObject {
				prototype: _,
				referenced_in_scope_as,
				position: _,
				is_function_this,
			} => {
				if *is_function_this {
					write!(buf, "create this function object").unwrap();
				} else {
					write!(buf, "create object as {referenced_in_scope_as:?}").unwrap();
				}
			}
			Event::Iterate { iterate_over, initial: _, kind: _ } => {
				buf.push_str("iterate\n");
				debug_effects(buf, iterate_over, types, info, debug);
				buf.push_str("end");
			}
			Event::FinalEvent(FinalEvent::Throw { thrown, .. }) => {
				buf.push_str("throw ");
				print_type_into_buf(*thrown, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::FinalEvent(FinalEvent::Break { .. }) => {
				buf.push_str("break");
			}
			Event::FinalEvent(FinalEvent::Continue { .. }) => {
				buf.push_str("continue");
			}
			Event::FinalEvent(FinalEvent::Return { returned, returned_position: _ }) => {
				buf.push_str("return ");
				print_type_into_buf(*returned, buf, &mut HashSet::new(), args, types, info, debug);
			}
		}
		buf.push('\n');
	}
}

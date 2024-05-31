use iterator_endiate::EndiateIteratorExt;
use source_map::{Nullable, SpanWithSource};
use std::collections::HashSet;

use super::{properties::PropertyKey, GenericChain, PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::{
		information::{get_value_of_constant_import_variable, InformationChain},
		Logical,
	},
	events::{Event, FinalEvent},
	features::{
		functions::ThisValue,
		objects::{Proxy, SpecialObjects},
	},
	types::{
		generics::generic_type_arguments::GenericArguments,
		get_constraint,
		properties::{get_properties_on_single_type, get_property_unbound, Publicity},
		Constructor, FunctionEffect, GenericChainLink, ObjectNature, PartiallyAppliedGenerics,
		TypeRelationOperator,
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
			print_type_into_buf(*a, buf, cycles, args, types, info, debug);
			buf.push_str(" & ");
			print_type_into_buf(*b, buf, cycles, args, types, info, debug);
		}
		Type::Or(a, b) => {
			print_type_into_buf(*a, buf, cycles, args, types, info, debug);
			buf.push_str(" | ");
			print_type_into_buf(*b, buf, cycles, args, types, info, debug);
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::FunctionGeneric { name, .. }
			| PolyNature::MappedGeneric { name, .. }
			| PolyNature::StructureGeneric { name, constrained: _ } => {
				if let Some(structure_args) =
					args.and_then(|args| args.get_argument(ty, info, types))
				{
					if debug {
						if let PolyNature::FunctionGeneric { .. } = nature {
							write!(buf, "[fg {} {}, =]", ty.0, name).unwrap();
						}
					}
					for (more, arg) in structure_args.iter().nendiate() {
						print_type_into_buf(*arg, buf, cycles, args, types, info, debug);
						if more {
							buf.push_str(" | ");
						}
					}
				} else {
					if debug {
						if let PolyNature::FunctionGeneric { eager_fixed, .. }
						| PolyNature::MappedGeneric { eager_fixed, .. } = nature
						{
							let key = match nature {
								PolyNature::FunctionGeneric { .. } => "fg",
								PolyNature::MappedGeneric { .. } => "mg",
								_ => "??",
							};
							write!(buf, "[{key} {}, @ ", ty.0).unwrap();
							print_type_into_buf(
								*eager_fixed,
								buf,
								cycles,
								args,
								types,
								info,
								debug,
							);
							buf.push_str("] ");
						} else {
							write!(buf, "[sg {}]", ty.0).unwrap();
						}
					}
					buf.push_str(name);
				}
			}
			PolyNature::InferGeneric { name } => {
				buf.push_str("infer ");
				buf.push_str(name);
			}
			PolyNature::FreeVariable { based_on: to, .. } => {
				if debug {
					// FV = free variable
					write!(buf, "[FV {}] @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Parameter { fixed_to: to } => {
				if debug {
					write!(buf, "[param {}] @ ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Open(to) => {
				if debug {
					write!(buf, "[open {}] ", ty.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			PolyNature::Error(to) => {
				if debug {
					write!(buf, "[error {}] ", ty.0).unwrap();
				}
				buf.push_str("(error) ");
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
					write!(buf, "[catch variable {}] ", ty.0).unwrap();
				}
				print_type_into_buf(*constraint, buf, cycles, args, types, info, debug);
			}
		},
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { on, arguments }) => {
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
				if debug {
					write!(buf, "?#{} ", ty.0).unwrap();
					print_type_into_buf(*condition, buf, cycles, args, types, info, debug);
					buf.push_str("? ");
				}
				print_type_into_buf(*truthy_result, buf, cycles, args, types, info, debug);
				buf.push_str(if debug { " : " } else { " | " });
				print_type_into_buf(*otherwise_result, buf, cycles, args, types, info, debug);
			}
			Constructor::KeyOf(on) => {
				// if get_constraint(*on, types).is_some() {
				// TODO try get and |
				buf.push_str("keyof ");
				print_type_into_buf(*on, buf, cycles, args, types, info, debug);
				// } else {
				// 	let properties = get_properties_on_type(*on, types, info);
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
			Constructor::Property { on, under, result, bind_this: _ } => {
				if crate::types::is_explicit_generic(*on, types) {
					print_type_into_buf(*on, buf, cycles, args, types, info, debug);
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
					get_constraint(*on, types).map(|ty| types.get_type_by_id(ty))
				{
					let new_arguments = sgs.arguments.clone();
					let args = GenericChainLink::append(ty, args.as_ref(), &new_arguments);
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				} else {
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info, debug);
					write!(buf, " {operator:?} ").unwrap();
					print_type_into_buf(*rhs, buf, cycles, args, types, info, debug);
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, info, debug);
					match operator {
							crate::features::operations::CanonicalEqualityAndInequality::StrictEqual => {
								buf.push_str(" === ");
							}
							crate::features::operations::CanonicalEqualityAndInequality::LessThan => {
								buf.push_str(" < ");
							}
						}
					print_type_into_buf(*rhs, buf, cycles, args, types, info, debug);
				}
				Constructor::UnaryOperator { operator, operand } => {
					write!(buf, "{operator:?} ").unwrap();
					print_type_into_buf(*operand, buf, cycles, args, types, info, debug);
				}
				Constructor::TypeOperator(to) => {
					write!(buf, "TypeOperator = {to:?}").unwrap();
				}
				Constructor::TypeRelationOperator(TypeRelationOperator::Extends {
					item,
					extends,
				}) => {
					print_type_into_buf(*item, buf, cycles, args, types, info, debug);
					buf.push_str(" extends ");
					print_type_into_buf(*extends, buf, cycles, args, types, info, debug);
				}
				Constructor::Image { on: _, with: _, result } => {
					write!(buf, "[func result {}] (*args here*)", ty.0).unwrap();
					// TODO arguments
					buf.push_str(" -> ");
					print_type_into_buf(*result, buf, cycles, args, types, info, debug);
				}
				Constructor::Property { on, under, result, bind_this } => {
					buf.push('(');
					print_type_into_buf(*on, buf, cycles, args, types, info, debug);
					buf.push_str(")[");
					print_property_key_into_buf(under, buf, cycles, args, types, info, debug);
					buf.push(']');
					if !bind_this {
						buf.push_str(" no bind");
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
			_constructor => {
				let base = get_constraint(ty, types).unwrap();
				print_type_into_buf(base, buf, cycles, args, types, info, debug);
			}
		},
		t @ (Type::Class { name, parameters: _, .. }
		| Type::Interface { name, parameters: _, .. }
		| Type::AliasTo { to: _, name, parameters: _ }) => {
			if debug {
				write!(buf, "{name}#{}", ty.0).unwrap();
			} else {
				buf.push_str(name);
			}
			if let (true, Type::AliasTo { to, .. }) = (debug, t) {
				buf.push_str(" to ");
				print_type_into_buf(*to, buf, cycles, args, types, info, debug);
			}
			// TODO
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
					print_type_into_buf(*p, buf, cycles, args, types, info, debug);
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
					write!(buf, "[obj {}]", ty.0).unwrap();
				} else {
					write!(buf, "[aol {}]", ty.0).unwrap();
				}
			}
			let prototype =
				info.get_chain_of_info().find_map(|info| info.prototypes.get(&ty).copied());

			if let Some(TypeId::ARRAY_TYPE) = prototype {
				if let Some(n) = get_array_length(info, ty, types) {
					buf.push('[');
					for i in 0..(n.into_inner() as usize) {
						if i != 0 {
							buf.push_str(", ");
						}
						let value = get_simple_value(info, ty, &PropertyKey::from_usize(i), types);

						if let Some(value) = value {
							print_type_into_buf(value, buf, cycles, args, types, info, debug);
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
					// crate::utilities::notify!("P during print {:?}", prototype);
					buf.push('[');
					print_type_into_buf(prototype, buf, cycles, args, types, info, debug);
					buf.push_str("] ");
				} else {
					// crate::utilities::notify!("no P on {:?} during print", id);
				}
				buf.push_str("{ ");
				let properties = get_properties_on_single_type(ty, types, info);
				for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
					if let Publicity::Private = publicity {
						buf.push('#');
					}

					let root;
					let args = if let Some((id, to)) = key.mapped_generic_id(types) {
						let mut map = crate::Map::default();
						map.insert(id, (to, SpanWithSource::NULL));
						root = GenericArguments::ExplicitRestrictions(map);
						Some(GenericChainLink::append_to_link(id, args.as_ref(), &root))
					} else {
						args
					};

					match value {
						PropertyValue::Value(value) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(": ");
							print_type_into_buf(value, buf, cycles, args, types, info, debug);
						}
						PropertyValue::Getter(_) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(": (getter)");
						}
						PropertyValue::Setter(_) => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(": (setter)");
						}
						PropertyValue::Deleted => {
							print_property_key_into_buf(
								&key, buf, cycles, args, types, info, debug,
							);
							buf.push_str(": never");
						}
						PropertyValue::ConditionallyExists { on: _, truthy } => {
							if let PropertyValue::Value(value) = *truthy {
								print_property_key_into_buf(
									&key, buf, cycles, args, types, info, debug,
								);
								buf.push_str("?: ");
								print_type_into_buf(value, buf, cycles, args, types, info, debug);
							} else {
								todo!()
							}
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
			SpecialObjects::Promise { events: () } => todo!(),
			SpecialObjects::Generator { position: () } => todo!(),
			SpecialObjects::Proxy(Proxy { handler, over }) => {
				// Copies from node behavior
				buf.push_str("Proxy [ ");
				print_type_into_buf(*over, buf, cycles, args, types, info, debug);
				buf.push_str(", ");
				print_type_into_buf(*handler, buf, cycles, args, types, info, debug);
				buf.push_str(" ]");
			}
			SpecialObjects::Import(exports) => {
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
					};
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
			SpecialObjects::RegularExpression(exp) => {
				buf.push('/');
				buf.push_str(exp);
				buf.push('/');
			}
			SpecialObjects::Function(..) => unreachable!(),
			SpecialObjects::ClassConstructor { name, prototype, constructor: _ } => {
				if debug {
					write!(buf, "constructor(for#{})@{name}#{}", prototype.0, ty.0).unwrap();
				} else {
					buf.push_str(name);
				}
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

	get_property_unbound((on, None), (Publicity::Public, property, None), ctx, types)
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
	depth: u8,
	debug: bool,
) {
	use std::fmt::Write;

	let args = GenericChain::None;

	let mut idx = 0;

	while idx < events.len() {
		for _ in 0..depth {
			buf.push('\t');
		}
		let event = &events[idx];
		match event {
			Event::ReadsReference { reference, reflects_dependency, position: _ } => {
				write!(buf, "read '{reference:?}' into {reflects_dependency:?}").unwrap();
			}
			Event::SetsVariable(variable, value, _) => {
				write!(buf, "{variable:?}' = ").unwrap();
				print_type_into_buf(*value, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::Getter {
				on,
				under,
				reflects_dependency,
				publicity: _,
				position: _,
				bind_this: _,
			} => {
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
				possibly_thrown: _,
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
			Event::Conditionally { condition, truthy_events, otherwise_events, position: _ } => {
				let truthy_events = *truthy_events as usize;
				let otherwise_events = *otherwise_events as usize;

				buf.push_str("if ");
				print_type_into_buf(*condition, buf, &mut HashSet::new(), args, types, info, debug);
				buf.push_str(" then \n");
				let events_if_true = &events[(idx + 1)..=(idx + truthy_events)];
				debug_effects(buf, events_if_true, types, info, depth + 1, debug);
				if otherwise_events != 0 {
					let start = idx + truthy_events + 1;
					let otherwise = &events[(start)..(start + otherwise_events)];
					buf.push_str(" else \n");
					debug_effects(buf, otherwise, types, info, depth + 1, debug);
				}
				idx += truthy_events + otherwise_events + 1;
				continue;
			}
			Event::CreateObject { prototype: _, referenced_in_scope_as, position: _ } => {
				write!(buf, "create object as {referenced_in_scope_as:?}").unwrap();
			}
			Event::Iterate { iterate_over, initial: _, kind: _ } => {
				buf.push_str("iterate\n");
				let inner_events = &events[(idx + 1)..(idx + 1 + *iterate_over as usize)];
				debug_effects(buf, inner_events, types, info, depth + 1, debug);
				idx += *iterate_over as usize + 1;
				continue;
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
			Event::FinalEvent(FinalEvent::Return { returned, position: _ }) => {
				buf.push_str("return ");
				print_type_into_buf(*returned, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::ExceptionTrap { .. } => todo!(),
			Event::RegisterVariable { .. } => {
				buf.push_str("register variable");
			}
			Event::EndOfControlFlow(_) => {
				buf.push_str("end");
			}
		}
		buf.push('\n');
		idx += 1;
	}
}

use iterator_endiate::EndiateIteratorExt;
use std::collections::HashSet;

use super::{properties::PropertyKey, PolyNature, Type, TypeArguments, TypeId, TypeStore};
use crate::{
	behavior::objects::SpecialObjects,
	context::{facts::Publicity, get_on_ctx, Logical},
	events::Event,
	types::{get_constraint, Constructor, StructureGenerics},
	Constant, GeneralContext, PropertyValue,
};

#[must_use]
pub fn print_type(id: TypeId, types: &TypeStore, ctx: &GeneralContext, debug: bool) -> String {
	let mut buf = String::new();
	print_type_into_buf(id, &mut buf, &mut HashSet::new(), None, types, ctx, debug);
	buf
}

#[must_use]
pub fn print_type_with_generics(
	id: TypeId,
	type_arguments: Option<&TypeArguments>,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) -> String {
	let mut buf = String::new();
	print_type_into_buf(id, &mut buf, &mut HashSet::new(), type_arguments, types, ctx, debug);
	buf
}

/// Recursion safe + reuses buffer
fn print_type_into_buf(
	id: TypeId,
	buf: &mut String,
	cycles: &mut HashSet<TypeId>,
	args: Option<&TypeArguments>,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) {
	use std::fmt::Write;

	let not_in_cycle = cycles.insert(id);
	if !not_in_cycle {
		buf.push_str("*cycle*");
		return;
	}

	let ty = types.get_type_by_id(id);
	match ty {
		Type::AliasTo { to: _, name, parameters: _ } => {
			buf.push_str(name);
		}
		Type::And(a, b) => {
			print_type_into_buf(*a, buf, cycles, args, types, ctx, debug);
			buf.push_str(" & ");
			print_type_into_buf(*b, buf, cycles, args, types, ctx, debug);
		}
		Type::Or(a, b) => {
			print_type_into_buf(*a, buf, cycles, args, types, ctx, debug);
			buf.push_str(" | ");
			print_type_into_buf(*b, buf, cycles, args, types, ctx, debug);
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::Generic { name, eager_fixed } => {
				if let Some(value) = args.and_then(|a| a.get(&id).map(|(t, _)| *t)) {
					print_type_into_buf(value, buf, cycles, args, types, ctx, debug);
				} else if debug {
					// TODO restriction
					write!(buf, "[generic {} {}, fixed to ", name, id.0).unwrap();
					print_type_into_buf(*eager_fixed, buf, cycles, args, types, ctx, debug);
					buf.push(']');
				} else {
					buf.push_str(name);
				}
			}
			PolyNature::FreeVariable { based_on: to, reference, .. } => {
				if debug {
					let name = reference.get_name(ctx);
					// FV = free variable
					write!(buf, "[FV '{}' {}] fixed to ", name, id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, ctx, debug);
			}
			PolyNature::Parameter { fixed_to: to } => {
				if debug {
					write!(buf, "[param {}] fixed to ", id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, ctx, debug);
			}
			PolyNature::Open(to) => {
				if debug {
					write!(buf, "[open {}] ", id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, args, types, ctx, debug);
			}
			PolyNature::RecursiveFunction(_, _) => {
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
				else_result,
				result_union: _,
			} => {
				if debug {
					write!(buf, "[? {id:? }").unwrap();
					print_type_into_buf(*condition, buf, cycles, args, types, ctx, debug);
					buf.push(']');
				}
				print_type_into_buf(*truthy_result, buf, cycles, args, types, ctx, debug);
				buf.push_str(if debug { " : " } else { " | " });
				print_type_into_buf(*else_result, buf, cycles, args, types, ctx, debug);
			}
			Constructor::StructureGenerics(StructureGenerics { on, arguments }) => {
				if debug {
					buf.push_str("SG (");
					print_type_into_buf(*on, buf, cycles, args, types, ctx, debug);
					buf.push(')');
				} else {
					print_type_into_buf(*on, buf, cycles, args, types, ctx, debug);
				}
				if debug && !arguments.closures.is_empty() {
					write!(buf, " [closures {:?}]", arguments.closures).unwrap();
				}
				if matches!(
					types.get_type_by_id(*on),
					Type::Interface { .. } | Type::Function { .. }
				) && !arguments.type_arguments.is_empty()
				{
					// TODO might be out of order ...
					buf.push('<');
					for (not_at_end, (arg, _)) in arguments.type_arguments.values().nendiate() {
						print_type_into_buf(*arg, buf, cycles, args, types, ctx, debug);
						if not_at_end {
							buf.push_str(", ");
						}
					}
					buf.push('>');
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, ctx, debug);
					buf.write_fmt(format_args!(" {operator:?} ")).unwrap();
					print_type_into_buf(*rhs, buf, cycles, args, types, ctx, debug);
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, args, types, ctx, debug);
					match operator {
							crate::behavior::operations::CanonicalEqualityAndInequality::StrictEqual => {
								buf.push_str(" === ");
							}
							crate::behavior::operations::CanonicalEqualityAndInequality::LessThan => {
								buf.push_str(" < ");
							}
						}
					print_type_into_buf(*rhs, buf, cycles, args, types, ctx, debug);
				}
				Constructor::UnaryOperator { operator: _, operand: _ } => todo!(),
				Constructor::TypeOperator(_) => todo!(),
				Constructor::TypeRelationOperator(_) => todo!(),
				Constructor::Image { on: _, with: _, result } => {
					// TODO arguments and stuff
					buf.push_str("[func result] ");
					print_type_into_buf(*result, buf, cycles, args, types, ctx, debug);
				}
				Constructor::Property { on, under, result: _ } => {
					print_type_into_buf(*on, buf, cycles, args, types, ctx, debug);
					buf.push('[');
					print_property_key_into_buf(buf, under, cycles, args, types, ctx, debug);
					buf.push(']');
				}
				Constructor::StructureGenerics { .. } | Constructor::ConditionalResult { .. } => {
					unreachable!()
				}
			},
			Constructor::Property { on, under, result } => {
				if crate::types::is_explicit_generic(*on, types) {
					print_type_into_buf(*on, buf, cycles, args, types, ctx, debug);
					buf.push('[');
					match under {
						PropertyKey::String(s) => {
							buf.push('"');
							buf.push_str(s);
							buf.push('"');
						}
						PropertyKey::Type(t) => {
							print_type_into_buf(*t, buf, cycles, args, types, ctx, debug);
						}
					};
					buf.push(']');
				} else {
					print_type_into_buf(*result, buf, cycles, args, types, ctx, debug);
				}
			}
			_constructor => {
				let base = get_constraint(id, types).unwrap();
				print_type_into_buf(base, buf, cycles, args, types, ctx, debug);
			}
		},
		Type::Interface { name, parameters, nominal } => {
			if debug && id.0 as usize > TypeId::INTERNAL_TYPE_COUNT {
				write!(buf, "(r{} nom={:?}) {name}", id.0, nominal).unwrap();
			} else {
				buf.push_str(name);
			}
			if let (true, Some(parameters)) = (debug, parameters) {
				buf.push('<');
				for param in parameters {
					print_type_into_buf(*param, buf, cycles, args, types, ctx, debug);
					buf.push_str(", ");
				}
				buf.push('>');
			}
		}
		Type::Constant(cst) => {
			if debug {
				write!(buf, "({}) {}", id.0, cst.as_type_name()).unwrap();
			} else {
				buf.push_str(&cst.as_type_name());
			}
		}
		Type::FunctionReference(func_id, this_ty) | Type::Function(func_id, this_ty) => {
			let func = types.functions.get(func_id).unwrap();
			if debug {
				write!(
					buf,
					"[FR #{} func, fvs {:?}, co {:?}, this {:?}, const {:?}] ",
					id.0,
					func.free_variables,
					func.closed_over_variables,
					this_ty,
					func.constant_function.as_deref().unwrap_or("-")
				)
				.unwrap();
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
			buf.push('(');
			for (not_at_end, param) in func.parameters.parameters.iter().nendiate() {
				buf.push_str(&param.name);
				buf.push_str(": ");
				print_type_into_buf(param.ty, buf, cycles, args, types, ctx, debug);
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push_str(") => ");
			print_type_into_buf(func.return_type, buf, cycles, args, types, ctx, debug);
		}
		Type::Object(..) => {
			if debug {
				write!(buf, "[obj {}]", id.0).unwrap();
			}
			if let Some(prototype) = get_on_ctx!(ctx.facts.prototypes.get(&id)) {
				if *prototype == TypeId::ARRAY_TYPE && !debug {
					if let Some(n) = get_array_length(ctx, id, types) {
						buf.push('[');
						for i in 0..(n.into_inner() as usize) {
							if i != 0 {
								buf.push_str(", ");
							}
							let value =
								get_simple_value(ctx, id, PropertyKey::from_usize(i), types)
									.expect("Trying to print complex array type");
							print_type_into_buf(value, buf, cycles, args, types, ctx, debug);
						}
						buf.push(']');
					} else {
						// TODO get property
						write!(buf, "Array").unwrap();
					}
					return;
				}

				buf.push('[');
				print_type_into_buf(*prototype, buf, cycles, args, types, ctx, debug);
				buf.push_str("] ");
			}
			buf.push_str("{ ");
			let properties = get_on_ctx!(ctx.get_properties_on_type(id));
			for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
				if let Publicity::Private = publicity {
					buf.push('#');
				}
				print_property_key_into_buf(buf, &key, cycles, args, types, ctx, debug);
				buf.push_str(": ");
				print_type_into_buf(value, buf, cycles, args, types, ctx, debug);
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push_str(" }");
		}
		Type::SpecialObject(special_object) => match special_object {
			SpecialObjects::Promise { events: () } => todo!(),
			SpecialObjects::Generator { position: () } => todo!(),
			SpecialObjects::Proxy { handler, over } => {
				// Copies from node behavior
				buf.push_str("Proxy [ ");
				print_type_into_buf(*over, buf, cycles, args, types, ctx, debug);
				buf.push_str(", ");
				print_type_into_buf(*handler, buf, cycles, args, types, ctx, debug);
				buf.push_str(" ]");
			}
			SpecialObjects::Import(exports) => {
				buf.push_str("{ ");
				for (not_at_end, (key, (variable, mutability))) in exports.named.iter().nendiate() {
					buf.push_str(key);
					buf.push_str(": ");
					match mutability {
						crate::behavior::variables::VariableMutability::Constant => {
							let value =
								get_on_ctx!(ctx.get_value_of_constant_import_variable(*variable));
							print_type_into_buf(value, buf, cycles, args, types, ctx, debug);
						}
						crate::behavior::variables::VariableMutability::Mutable {
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
		},
	}

	cycles.remove(&id);
}

fn get_simple_value(
	ctx: &GeneralContext,
	on: TypeId,
	property: PropertyKey,
	types: &TypeStore,
) -> Option<TypeId> {
	get_on_ctx!(ctx.get_property_unbound(on, Publicity::Public, property, types)).and_then(|v| {
		if let Logical::Pure(PropertyValue::Value(t)) = v {
			Some(t)
		} else {
			None
		}
	})
}

fn get_array_length(
	ctx: &GeneralContext,
	on: TypeId,
	types: &TypeStore,
) -> Option<ordered_float::NotNan<f64>> {
	let id = get_simple_value(
		ctx,
		on,
		PropertyKey::String(std::borrow::Cow::Borrowed("length")),
		types,
	)?;
	if let Type::Constant(Constant::Number(n)) = types.get_type_by_id(id) {
		Some(*n)
	} else {
		None
	}
}

#[must_use]
pub fn print_property_key(
	key: &PropertyKey,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) -> String {
	let mut string = String::new();
	print_property_key_into_buf(&mut string, key, &mut HashSet::new(), None, types, ctx, debug);
	string
}

pub(crate) fn print_property_key_into_buf(
	buf: &mut String,
	key: &PropertyKey,
	cycles: &mut HashSet<TypeId>,
	args: Option<&TypeArguments>,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) {
	match key {
		PropertyKey::String(s) => buf.push_str(s),
		PropertyKey::Type(t) => {
			buf.push('[');
			print_type_into_buf(*t, buf, cycles, args, types, ctx, debug);
			buf.push(']');
		}
	}
}

pub fn debug_effects(
	buf: &mut String,
	events: &[Event],
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) {
	use std::fmt::Write;

	for event in events {
		match event {
			Event::ReadsReference { reference, reflects_dependency, position: _ } => {
				let name = reference.get_name(ctx);
				buf.write_fmt(format_args!("read '{name}' into {reflects_dependency:?}")).unwrap();
			}
			Event::SetsVariable(variable, value, _) => {
				let on = get_on_ctx!(ctx.get_variable_name(*variable));
				buf.push_str(on);
				buf.push_str(" = ");
				print_type_into_buf(*value, buf, &mut HashSet::new(), None, types, ctx, debug);
			}
			Event::Getter { on, under, reflects_dependency, publicity: _, position: _ } => {
				buf.push_str("read ");
				print_type_into_buf(*on, buf, &mut HashSet::new(), None, types, ctx, debug);
				if let PropertyKey::String(_) = under {
					buf.push('.');
				}
				print_property_key_into_buf(
					buf,
					under,
					&mut HashSet::new(),
					None,
					types,
					ctx,
					debug,
				);
				buf.write_fmt(format_args!(" into {reflects_dependency:?}")).unwrap();
			}
			Event::Setter { on, under, new, initialization, publicity: _, position: _ } => {
				if *initialization {
					buf.write_fmt(format_args!("initialise {:?} with ", *on)).unwrap();
					if let PropertyValue::Value(new) = new {
						print_type_into_buf(
							*new,
							buf,
							&mut HashSet::new(),
							None,
							types,
							ctx,
							debug,
						);
					}
				} else {
					print_type_into_buf(*on, buf, &mut HashSet::new(), None, types, ctx, debug);
					buf.push('[');
					print_property_key_into_buf(
						buf,
						under,
						&mut HashSet::default(),
						None,
						types,
						ctx,
						debug,
					);
					buf.push_str("] = ");
					if let PropertyValue::Value(new) = new {
						print_type_into_buf(
							*new,
							buf,
							&mut HashSet::new(),
							None,
							types,
							ctx,
							debug,
						);
					}
				}
			}
			Event::CallsType {
				on,
				with: _,
				reflects_dependency: _,
				timing,
				called_with_new: _,
				position: _,
			} => {
				buf.push_str("call ");
				print_type_into_buf(*on, buf, &mut HashSet::new(), None, types, ctx, debug);
				buf.push_str(match timing {
					crate::events::CallingTiming::Synchronous => "now",
					crate::events::CallingTiming::QueueTask => "queue",
					crate::events::CallingTiming::AtSomePointManyTimes => "sometime",
				});
				// TODO args
			}
			Event::Throw(value, _) => {
				buf.push_str("throw ");
				print_type_into_buf(*value, buf, &mut HashSet::new(), None, types, ctx, debug);
			}
			Event::Conditionally { condition, events_if_truthy, else_events, position: _ } => {
				buf.push_str("if ");
				print_type_into_buf(*condition, buf, &mut HashSet::new(), None, types, ctx, debug);
				buf.push_str(" then ");
				debug_effects(buf, events_if_truthy, types, ctx, debug);
				if !else_events.is_empty() {
					buf.push_str(" else ");
					debug_effects(buf, else_events, types, ctx, debug);
				}
			}
			Event::Return { returned, returned_position: _ } => {
				buf.push_str("return ");
				print_type_into_buf(*returned, buf, &mut HashSet::new(), None, types, ctx, debug);
			}
			Event::CreateObject {
				prototype: _,
				referenced_in_scope_as,
				position: _,
				is_function_this,
			} => {
				if *is_function_this {
					buf.write_fmt(format_args!("create this function object")).unwrap();
				} else {
					buf.write_fmt(format_args!("create object as {referenced_in_scope_as:?}"))
						.unwrap();
				}
			}
			Event::Break { .. } => {
				buf.push_str("break");
			}
			Event::Continue { .. } => {
				buf.push_str("continue");
			}
			Event::Iterate { iterate_over, initial: _, kind: _ } => {
				buf.push_str("iterate\n");
				debug_effects(buf, iterate_over, types, ctx, debug);
				buf.push_str("end");
			}
		}
		buf.push('\n');
	}
}

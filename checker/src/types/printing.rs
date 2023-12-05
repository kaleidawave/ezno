use iterator_endiate::EndiateIteratorExt;
use std::collections::HashSet;

use super::{properties::PropertyKey, PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::{facts::Publicity, get_on_ctx},
	types::{Constructor, StructureGenerics},
	Constant, GeneralContext,
};

/// TODO temp, needs recursion safe, reuse buffer

#[must_use]
pub fn print_type(id: TypeId, types: &TypeStore, ctx: &GeneralContext, debug: bool) -> String {
	let mut buf = String::new();
	print_type_into_buf(id, &mut buf, &mut HashSet::new(), types, ctx, debug);

	buf
}

fn print_type_into_buf(
	id: TypeId,
	buf: &mut String,
	cycles: &mut HashSet<TypeId>,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) {
	use std::fmt::Write;

	let not_in_cycle = cycles.insert(id);
	if !not_in_cycle {
		buf.push_str("cyclic");
		return;
	}

	let ty = types.get_type_by_id(id);
	match ty {
		Type::AliasTo { to, name, parameters } => {
			buf.push_str(name);
		}
		Type::And(a, b) => {
			print_type_into_buf(*a, buf, cycles, types, ctx, debug);
			buf.push_str(" & ");
			print_type_into_buf(*b, buf, cycles, types, ctx, debug);
		}
		Type::Or(a, b) => {
			print_type_into_buf(*a, buf, cycles, types, ctx, debug);
			buf.push_str(" | ");
			print_type_into_buf(*b, buf, cycles, types, ctx, debug);
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::Generic { name, eager_fixed } => {
				if debug {
					// TODO restriction
					write!(buf, "[generic {} {}, fixed to ", name, id.0).unwrap();
					print_type_into_buf(*eager_fixed, buf, cycles, types, ctx, debug);
					buf.push(']');
				} else {
					buf.push_str(name);
				}
			}
			PolyNature::FreeVariable { based_on: to, reference, .. } => {
				if debug {
					let name = reference.get_name(ctx);
					// FV = free variable
					write!(buf, "[FV {} {}]", name, id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, types, ctx, debug);
			}
			PolyNature::Parameter { fixed_to: to } => {
				if debug {
					write!(buf, "[param {}]", id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, types, ctx, debug);
			}
			PolyNature::Open(to) => {
				if debug {
					write!(buf, "[open {}]", id.0).unwrap();
				}
				print_type_into_buf(*to, buf, cycles, types, ctx, debug);
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
				result_union,
			} => {
				if debug {
					write!(buf, "[? {id:? }").unwrap();
					print_type_into_buf(*condition, buf, cycles, types, ctx, debug);
					buf.push(']');
				}
				print_type_into_buf(*truthy_result, buf, cycles, types, ctx, debug);
				buf.push_str(if debug { " : " } else { " | " });
				print_type_into_buf(*else_result, buf, cycles, types, ctx, debug);
			}
			Constructor::StructureGenerics(StructureGenerics { on, arguments }) => {
				if debug {
					buf.push_str("SG (");
					print_type_into_buf(*on, buf, cycles, types, ctx, debug);
					buf.push(')');
				} else {
					print_type_into_buf(*on, buf, cycles, types, ctx, debug);
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
						print_type_into_buf(*arg, buf, cycles, types, ctx, debug);
						if not_at_end {
							buf.push_str(", ");
						}
					}
					buf.push('>');
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, types, ctx, debug);
					buf.push_str(" + ");
					print_type_into_buf(*rhs, buf, cycles, types, ctx, debug);
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					print_type_into_buf(*lhs, buf, cycles, types, ctx, debug);
					match operator {
							crate::behavior::operations::CanonicalEqualityAndInequality::StrictEqual => {
								buf.push_str(" === ");
							}
							crate::behavior::operations::CanonicalEqualityAndInequality::LessThan => {
								buf.push_str(" > ");
							}
						}
					print_type_into_buf(*rhs, buf, cycles, types, ctx, debug);
				}
				Constructor::UnaryOperator { operator, operand } => todo!(),
				Constructor::TypeOperator(_) => todo!(),
				Constructor::TypeRelationOperator(_) => todo!(),
				Constructor::FunctionResult { on, with, result } => {
					// TODO arguments and stuff
					buf.push_str("[func result] ");
					print_type_into_buf(*result, buf, cycles, types, ctx, debug);
				}
				Constructor::Property { on, under, result: _ } => {
					print_type_into_buf(*on, buf, cycles, types, ctx, debug);
					buf.push('[');
					print_property_key_into_buf(buf, under, cycles, types, ctx, debug);
					buf.push(']');
				}
				Constructor::StructureGenerics { .. } | Constructor::ConditionalResult { .. } => {
					unreachable!()
				}
			},
			Constructor::Property { on, under, result } => {
				if crate::types::is_explicit_generic(*on, types) {
					print_type_into_buf(*on, buf, cycles, types, ctx, debug);
					buf.push('[');
					match under {
						PropertyKey::String(s) => {
							buf.push('"');
							buf.push_str(s);
							buf.push('"');
						}
						PropertyKey::Type(t) => {
							print_type_into_buf(*t, buf, cycles, types, ctx, debug);
						}
					};
					buf.push(']');
				} else {
					print_type_into_buf(*result, buf, cycles, types, ctx, debug);
				}
			}
			constructor => {
				let base = get_on_ctx!(ctx.get_poly_base(id, types)).unwrap();
				print_type_into_buf(base, buf, cycles, types, ctx, debug);
			}
		},
		Type::Interface { name, parameters, nominal } => {
			if debug {
				write!(buf, "(r{} nom={:?}) {name}", id.0, nominal).unwrap();
			// buf.push_str("{ ");
			// let properties = get_on_ctx!(ctx.get_properties_on_type(id));
			// for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
			// 	if let PublicityKind::Private = publicity {
			// 		buf.push('#');
			// 	}
			// 	print_property_key_into_buf(buf, &key, cycles, types, ctx, debug);
			// 	buf.push_str(": ");
			// 	print_type_into_buf(value, buf, cycles, types, ctx, debug);
			// 	if not_at_end {
			// 		buf.push_str(", ");
			// 	}
			// }
			// buf.push_str(" }")
			} else {
				buf.push_str(name);
			}
			if let (true, Some(parameters)) = (debug, parameters) {
				buf.push('<');
				for param in parameters {
					print_type_into_buf(*param, buf, cycles, types, ctx, debug);
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
					"[t{} func, fvs {:?}, co {:?}, this {:?}, const {:?}] ",
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
					if let Some(ref default) = param.default {
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
				print_type_into_buf(param.ty, buf, cycles, types, ctx, debug);
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push_str(") => ");
			print_type_into_buf(func.return_type, buf, cycles, types, ctx, debug);
		}
		Type::Object(..) => {
			if debug {
				write!(buf, "[obj {}]", id.0).unwrap();
			}
			if let Some(prototype) = get_on_ctx!(ctx.facts.prototypes.get(&id)) {
				buf.push('[');
				print_type_into_buf(*prototype, buf, cycles, types, ctx, debug);
				buf.push_str("] ");
			}
			buf.push_str("{ ");
			let properties = get_on_ctx!(ctx.get_properties_on_type(id));
			for (not_at_end, (publicity, key, value)) in properties.into_iter().nendiate() {
				if let Publicity::Private = publicity {
					buf.push('#');
				}
				print_property_key_into_buf(buf, &key, cycles, types, ctx, debug);
				buf.push_str(": ");
				print_type_into_buf(value, buf, cycles, types, ctx, debug);
				if not_at_end {
					buf.push_str(", ");
				}
			}
			buf.push_str(" }");
		}
		Type::SpecialObject(special_object) => match special_object {
			crate::behavior::objects::SpecialObjects::Promise { events } => todo!(),
			crate::behavior::objects::SpecialObjects::Generator { position } => todo!(),
			crate::behavior::objects::SpecialObjects::Proxy { handler, over } => {
				// Copies from node behavior
				buf.push_str("Proxy [ ");
				print_type_into_buf(*over, buf, cycles, types, ctx, debug);
				buf.push_str(", ");
				print_type_into_buf(*handler, buf, cycles, types, ctx, debug);
				buf.push_str(" ]");
			}
			crate::behavior::objects::SpecialObjects::Import(exports) => {
				buf.push_str("{ ");
				for (not_at_end, (key, (variable, mutability))) in exports.named.iter().nendiate() {
					buf.push_str(key);
					buf.push_str(": ");
					match mutability {
						crate::behavior::variables::VariableMutability::Constant => {
							let value =
								get_on_ctx!(ctx.get_value_of_constant_import_variable(*variable));
							print_type_into_buf(value, buf, cycles, types, ctx, debug);
						}
						crate::behavior::variables::VariableMutability::Mutable {
							reassignment_constraint,
						} => todo!(),
					};
					if not_at_end {
						buf.push_str(", ");
					}
				}
				buf.push_str(" }");
			}
		},
	}

	cycles.remove(&id);
}

#[must_use]
pub fn print_property_key(
	key: &PropertyKey,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) -> String {
	let mut string = String::new();
	print_property_key_into_buf(&mut string, key, &mut HashSet::new(), types, ctx, debug);
	string
}

pub(crate) fn print_property_key_into_buf(
	buf: &mut String,
	key: &PropertyKey,
	cycles: &mut HashSet<TypeId>,
	types: &TypeStore,
	ctx: &GeneralContext,
	debug: bool,
) {
	match key {
		PropertyKey::String(s) => buf.push_str(s),
		PropertyKey::Type(t) => print_type_into_buf(*t, buf, cycles, types, ctx, debug),
	}
}

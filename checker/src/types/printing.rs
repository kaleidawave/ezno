use std::collections::HashSet;

use super::{PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::{facts::PublicityKind, get_on_ctx},
	types::{Constructor, StructureGenerics},
	GeneralContext,
};

/// TODO temp, needs recursion safe, reuse buffer
pub fn print_type(id: TypeId, types: &TypeStore, ctx: &GeneralContext, debug: bool) -> String {
	let mut buf = String::new();
	print_type_into_buf(id, &mut buf, &mut HashSet::new(), types, ctx, debug);

	return buf;

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
				nature => {
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
					condition: on,
					truthy_result: true_result,
					else_result: false_result,
					result_union,
				} => {
					if debug {
						print_type_into_buf(*on, buf, cycles, types, ctx, debug);
						buf.push_str(" ? ");
					}
					print_type_into_buf(*true_result, buf, cycles, types, ctx, debug);
					buf.push_str(if debug { " : " } else { " | " });
					print_type_into_buf(*false_result, buf, cycles, types, ctx, debug);
				}
				Constructor::StructureGenerics(StructureGenerics { on, arguments }) => {
					if debug {
						buf.push('(');
						print_type_into_buf(*on, buf, cycles, types, ctx, debug);
						buf.push(')');
					} else {
						print_type_into_buf(*on, buf, cycles, types, ctx, debug);
					}
					if debug && !arguments.closures.is_empty() {
						write!(buf, " [closures {:?}]", arguments.closures).unwrap();
					}
					if !arguments.type_arguments.is_empty() {
						// TODO might be out of order ...
						buf.push('<');
						for (arg, _) in arguments.type_arguments.values() {
							print_type_into_buf(*arg, buf, cycles, types, ctx, debug);
							buf.push_str(", ");
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
					Constructor::Property { on, under } => {
						print_type_into_buf(*on, buf, cycles, types, ctx, debug);
						buf.push('[');
						print_type_into_buf(*under, buf, cycles, types, ctx, debug);
						buf.push(']');
					}
					Constructor::StructureGenerics { .. }
					| Constructor::ConditionalResult { .. } => {
						unreachable!()
					}
				},
				constructor => {
					let base = get_on_ctx!(ctx.get_poly_base(id, types)).unwrap();
					print_type_into_buf(base, buf, cycles, types, ctx, debug)
				}
			},
			Type::NamedRooted { name, parameters, nominal } => {
				if debug {
					write!(buf, "(r{} nom={:?}) {name}", id.0, nominal).unwrap();
				} else {
					buf.push_str(name)
				}
				if let (true, Some(parameters)) = (debug, parameters) {
					buf.push('<');
					for param in parameters {
						print_type_into_buf(*param, buf, cycles, types, ctx, debug);
						buf.push_str(", ");
					}
					buf.push('>')
				}
			}
			Type::Constant(cst) => {
				if debug {
					write!(buf, "({}) {}", id.0, cst.as_type_name()).unwrap();
				} else {
					buf.push_str(&cst.as_type_name());
				}
			}
			f @ Type::FunctionReference(func_id, this_ty)
			| f @ Type::Function(func_id, this_ty) => {
				let func = types.functions.get(func_id).unwrap();
				if debug {
					write!(
						buf,
						"[func {}/{:?}, uses {:?}, closes over {:?}, this {:?}, const {:?}]",
						id.0,
						func_id,
						func.free_variables,
						func.closed_over_variables,
						this_ty,
						func.constant_id
					)
					.unwrap();
				}
				if let Some(ref parameters) = func.type_parameters {
					buf.push('<');
					for param in parameters.0.iter() {
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
						buf.push_str(", ")
					}
					buf.push('>');
				}
				buf.push('(');
				for param in func.parameters.parameters.iter() {
					buf.push_str(&param.name);
					buf.push_str(": ");
					print_type_into_buf(param.ty, buf, cycles, types, ctx, debug);
					buf.push_str(", ");
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
				buf.push('{');
				for (key, publicity, value) in get_on_ctx!(ctx.get_properties_on_type(id)) {
					if let PublicityKind::Private = publicity {
						buf.push('#');
					}
					print_type_into_buf(key, buf, cycles, types, ctx, debug);
					buf.push_str(": ");
					print_type_into_buf(value, buf, cycles, types, ctx, debug);
					buf.push_str(", ");
				}
				buf.push('}');
			}
			Type::Class(..) => todo!("name"),
			Type::SpecialObject(special_object) => match special_object {
				crate::behavior::objects::SpecialObjects::Promise { events } => todo!(),
				crate::behavior::objects::SpecialObjects::Generator { position } => todo!(),
				crate::behavior::objects::SpecialObjects::Proxy { handler, over } => todo!(),
				crate::behavior::objects::SpecialObjects::Import(exports) => {
					buf.push('{');
					for (key, (variable, mutability)) in exports.named.iter() {
						buf.push('"');
						buf.push_str(key);
						buf.push_str("\": ");
						match mutability {
							crate::behavior::variables::VariableMutability::Constant => {
								let value = get_on_ctx!(
									ctx.get_value_of_constant_import_variable(*variable)
								);
								print_type_into_buf(value, buf, cycles, types, ctx, debug);
							}
							crate::behavior::variables::VariableMutability::Mutable {
								reassignment_constraint,
							} => todo!(),
						};
						buf.push_str(", ");
					}
					buf.push('}');
				}
			},
		}

		cycles.remove(&id);
	}
}

use super::{PolyNature, Type, TypeId, TypeStore};
use crate::{
	context::get_on_ctx,
	types::{Constructor, StructureGenerics},
	GeneralContext,
};

/// TODO temp, needs recursion safe, reuse buffer
pub fn print_type(id: TypeId, types: &TypeStore, ctx: &GeneralContext, debug: bool) -> String {
	use std::fmt::Write;

	let ty = types.get_type_by_id(id);
	// crate::utils::notify!("Printing {:?}", ty);
	match ty {
		Type::AliasTo { to, name, parameters } => name.clone(),
		Type::And(a, b) => {
			format!("{} & {}", print_type(*a, types, ctx, debug), print_type(*b, types, ctx, debug))
		}
		Type::Or(a, b) => {
			format!("{} | {}", print_type(*a, types, ctx, debug), print_type(*b, types, ctx, debug))
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::Generic { name, .. } => name.clone(),
			PolyNature::ParentScope { based_on: to, reference, .. } => {
				let on = print_type(*to, types, ctx, debug);
				if debug {
					let name = reference.get_name(ctx);
					// in parent scope
					format!("[ips {name} {}] {on}", id.0)
				} else {
					on
				}
			}
			PolyNature::Parameter { fixed_to: to } => {
				let on = print_type(*to, types, ctx, debug);
				if debug {
					format!("[param {}] {on}", id.0)
				} else {
					on
				}
			}
			PolyNature::Open(to) => {
				let on = print_type(*to, types, ctx, debug);
				if debug {
					format!("[open {}] {on}", id.0)
				} else {
					on
				}
			}
			nature => {
				todo!()
				// let modified_base = match env {
				// 	GeneralContext::Syntax(syn) => {
				// 		syn.parents_iter().find_map(|env| get_on_ctx!(env.bases.get(&id)).copied())
				// 	}
				// 	GeneralContext::Root(root) => root.bases.get(&id).copied(),
				// };

				// print_type(aliases, envtypes,, debug)
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
				let on = print_type(*on, types, ctx, debug);
				let true_result = print_type(*true_result, types, ctx, debug);
				let false_result = print_type(*false_result, types, ctx, debug);
				if debug {
					// TODO more
					format!("({on} ? {true_result} : {false_result})")
				} else {
					format!("{true_result} | {false_result}")
				}
			}
			Constructor::StructureGenerics(StructureGenerics { on, arguments }) => {
				let mut on = print_type(*on, types, ctx, debug);
				crate::utils::notify!("printing {:?}", id);
				if debug {
					if !arguments.closures.is_empty() {
						write!(on, "[closures {:?}]", arguments.closures).unwrap();
					}
				}
				if !arguments.type_arguments.is_empty() {
					let mut buf = String::new();
					// TODO might be out of order ...
					for arg in arguments.type_arguments.values() {
						let arg = print_type(*arg, types, ctx, debug);
						buf.push_str(&arg);
						buf.push_str(", ");
					}
					format!("{on}<{buf}>")
				} else {
					on
				}
			}
			constructor if debug => match constructor {
				Constructor::BinaryOperator { lhs, operator, rhs } => {
					let lhs = print_type(*lhs, types, ctx, debug);
					let rhs = print_type(*rhs, types, ctx, debug);
					format!("({lhs} + {rhs})")
				}
				Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
					let lhs = print_type(*lhs, types, ctx, debug);
					let rhs = print_type(*rhs, types, ctx, debug);
					match operator {
						crate::behavior::operations::CanonicalEqualityAndInequality::StrictEqual => {
							format!("({lhs} === {rhs})")
						}
						crate::behavior::operations::CanonicalEqualityAndInequality::LessThan => {
							format!("({lhs} > {rhs})")
						}
					}
				}
				Constructor::UnaryOperator { operator, operand } => todo!(),
				Constructor::TypeOperator(_) => todo!(),
				Constructor::TypeRelationOperator(_) => todo!(),
				Constructor::FunctionResult { on, with, result } => {
					// TODO arguments and stuff
					format!("[func result] {}", print_type(*result, types, ctx, debug))
				}
				Constructor::Property { on, under } => {
					let on = print_type(*on, types, ctx, debug);
					let under = print_type(*under, types, ctx, debug);
					format!("{on}[{under}]")
				}
				Constructor::StructureGenerics { .. } | Constructor::ConditionalResult { .. } => {
					unreachable!()
				}
			},
			constructor => {
				let base = get_on_ctx!(ctx.get_poly_base(id, types)).unwrap();
				print_type(base, types, ctx, debug)
			}
		},
		Type::NamedRooted { name, parameters } => {
			if parameters.is_some() {
				crate::utils::notify!("TODO print parameters");
			}
			if debug {
				format!("(r{}) {name}", id.0)
			} else {
				name.clone()
			}
		}
		Type::Constant(cst) => {
			if debug {
				format!("({}) {}", id.0, cst.as_type_name())
			} else {
				cst.as_type_name()
			}
		}
		f @ Type::FunctionReference(func_id, this_ty) | f @ Type::Function(func_id, this_ty) => {
			let func = types.functions.get(func_id).unwrap();
			let mut buf = String::new();
			if debug {
				write!(
					buf,
					"[func {}/{:?}, uses {:?}, closes over {:?}, this {:?}, const {:?}]",
					id.0,
					func_id,
					func.used_parent_references,
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
				buf.push_str(&print_type(param.ty, types, ctx, debug));
				buf.push_str(", ");
			}
			buf.push_str(") => ");
			buf.push_str(&print_type(func.return_type, types, ctx, debug));
			buf
		}
		Type::Object(..) => {
			let mut buf = String::new();
			if debug {
				write!(buf, "[obj {}]", id.0).unwrap();
			}
			if let Some(prototype) = get_on_ctx!(ctx.facts.prototypes.get(&id)) {
				buf.push('[');
				buf.push_str(&print_type(*prototype, types, ctx, debug));
				buf.push_str("] ");
			}
			buf.push('{');
			for (key, value) in get_on_ctx!(ctx.get_properties_on_type(id)) {
				buf.push_str(&print_type(key, types, ctx, debug));
				buf.push_str(": ");
				buf.push_str(&print_type(value, types, ctx, debug));
				buf.push_str(", ");
			}
			buf.push('}');
			buf
		}
		Type::Class(..) => todo!("name"),
	}
}

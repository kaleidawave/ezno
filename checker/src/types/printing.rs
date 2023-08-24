use crate::{context::get_on_ctx, GeneralContext};

use super::{PolyNature, Type, TypeId, TypeStore};

/// TODO temp, needs recursion safe, reuse buffer
pub fn print_type(id: TypeId, types: &TypeStore, env: &GeneralContext, debug: bool) -> String {
	let ty = types.get_type_by_id(id);
	// crate::utils::notify!("Printing {:?}", ty);
	match ty {
		Type::AliasTo { to, name, parameters } => name.clone(),
		Type::And(a, b) => {
			format!("{} & {}", print_type(*a, types, env, debug), print_type(*b, types, env, debug))
		}
		Type::Or(a, b) => {
			format!("{} | {}", print_type(*a, types, env, debug), print_type(*b, types, env, debug))
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::Generic { name, .. } => name.clone(),
			PolyNature::ParentScope { .. } | PolyNature::Parameter { .. } => {
				let ty = get_on_ctx!(env.get_poly_base(id, types)).unwrap().get_type();
				let on = print_type(ty, types, env, debug);
				if debug {
					let kind = match nature {
						PolyNature::Parameter { fixed_to } => "parameter",
						PolyNature::ParentScope { reference, based_on } => "parent scope",
						_ => unreachable!(),
					};
					format!("[{kind} {id:?}] {on}")
				} else {
					on
				}
			}
			PolyNature::Open(to) => {
				let on = print_type(*to, types, env, debug);
				if debug {
					format!("[open] {on}")
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
			super::Constructor::ConditionalTernary { on, true_res, false_res, result_union } => {
				let on = print_type(*result_union, types, env, debug);
				if debug {
					// TODO more
					format!("[ternary] {on}")
				} else {
					on
				}
			}
			super::Constructor::StructureGenerics { on, with } => todo!(),
			constructor if debug => match constructor {
				super::Constructor::BinaryOperator { lhs, operator, rhs } => {
					let lhs = print_type(*lhs, types, env, debug);
					let rhs = print_type(*rhs, types, env, debug);
					format!("({lhs} + {rhs})")
				}
				super::Constructor::RelationOperator { lhs, operator, rhs } => todo!(),
				super::Constructor::LogicalOperator { lhs, operator, rhs } => todo!(),
				super::Constructor::UnaryOperator { operator, operand } => todo!(),
				super::Constructor::TypeOperator(_) => todo!(),
				super::Constructor::TypeRelationOperator(_) => todo!(),
				super::Constructor::ConditionalTernary {
					on,
					true_res,
					false_res,
					result_union,
				} => todo!(),
				super::Constructor::FunctionResult { on, with, result } => {
					let a = match result {
						crate::types::PolyPointer::Fixed(fixed) => {
							print_type(*fixed, types, env, debug)
						}
						crate::types::PolyPointer::Inferred(_) => todo!(),
					};
					format!("[func result] {}", a)
				}
				super::Constructor::Property { on, under } => {
					let on = print_type(*on, types, env, debug);
					let under = print_type(*under, types, env, debug);
					format!("{on}[{under}]")
				}
				super::Constructor::StructureGenerics { on, with } => todo!(),
			},
			constructor => {
				let base = get_on_ctx!(env.get_poly_base(id, types)).unwrap().get_type();
				print_type(base, types, env, debug)
			}
		},
		Type::NamedRooted { name, parameters } => {
			if parameters.is_some() {
				crate::utils::notify!("TODO print parameters");
			}
			if debug {
				format!("(Root #{id:?}) {name}")
			} else {
				name.clone()
			}
		}
		Type::Constant(cst) => cst.as_type_name(),
		Type::Function(func, _) => {
			let mut buf = String::new();
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
				buf.push_str(&print_type(param.ty, types, env, debug));
				buf.push_str(", ");
			}
			buf.push_str(") => ");
			buf.push_str(&print_type(func.return_type, types, env, debug));
			buf
		}
		Type::Object(..) => {
			let mut buf = String::new();
			if let Some(prototype) = get_on_ctx!(env.prototypes.get(&id)) {
				buf.push('[');
				buf.push_str(&print_type(*prototype, types, env, debug));
				buf.push_str("] ");
			}
			buf.push('{');
			for (key, value) in get_on_ctx!(env.get_properties_on_type(id)) {
				buf.push_str(&print_type(key, types, env, debug));
				buf.push_str(": ");
				buf.push_str(&print_type(value, types, env, debug));
				buf.push_str(", ");
			}
			buf.push_str(" }");
			buf
		}
	}
}

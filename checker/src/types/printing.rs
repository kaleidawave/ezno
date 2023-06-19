use crate::{context::get_env, GeneralEnvironment};

use super::{PolyNature, Type, TypeId, TypeStore};

/// TODO temp, needs recursion safe, reuse buffer
pub fn print_type(types: &TypeStore, id: TypeId, env: &GeneralEnvironment) -> String {
	let ty = types.get_type_by_id(id);
	// crate::utils::notify!("Printing {:?}", ty);
	match ty {
		Type::AliasTo { to, name, parameters } => name.clone(),
		Type::And(a, b) => {
			format!("{} & {}", print_type(types, *a, env), print_type(types, *b, env))
		}
		Type::Or(a, b) => {
			format!("{} | {}", print_type(types, *a, env), print_type(types, *b, env))
		}
		Type::RootPolyType(nature) => match nature {
			PolyNature::Generic { name, .. } => name.clone(),
			PolyNature::ParentScope { .. } | PolyNature::Parameter { .. } => {
				let ty = get_env!(env.get_poly_base(id, types)).unwrap().get_type();
				print_type(types, ty, env)
			}
			PolyNature::Open(to) => print_type(types, *to, env),
			nature => {
				todo!()
				// let modified_base = match env {
				// 	GeneralEnvironment::Syntax(syn) => {
				// 		syn.parents_iter().find_map(|env| get_env!(env.bases.get(&id)).copied())
				// 	}
				// 	GeneralEnvironment::Root(root) => root.bases.get(&id).copied(),
				// };

				// print_type(types, aliases, env)
			}
		},
		// TODO these can vary
		Type::Constructor(constructor) => match constructor {
			super::Constructor::ConditionalTernary { on, true_res, false_res, result_union } => {
				print_type(types, *result_union, env)
			}
			super::Constructor::StructureGenerics { on, with } => todo!(),
			_ => {
				let base = get_env!(env.get_poly_base(id, types)).unwrap().get_type();
				print_type(types, base, env)
			}
		},
		Type::NamedRooted { name, parameters } => {
			if parameters.is_some() {
				todo!()
			}
			name.clone()
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
				buf.push_str(&print_type(types, param.ty, env));
				buf.push_str(", ");
			}
			buf.push_str(") => ");
			buf.push_str(&print_type(types, func.return_type, env));
			buf
		}
		Type::Object(..) => {
			let mut buf = String::from("{ ");
			for (key, value) in get_env!(env.get_properties_on_type(id)) {
				buf.push_str(&print_type(types, key, env));
				buf.push_str(": ");
				buf.push_str(&print_type(types, value, env));
				buf.push_str(", ");
			}
			buf.push_str(" }");
			buf
		}
	}
}

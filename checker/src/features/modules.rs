use super::variables::{VariableMutability, VariableOrImport};
use crate::{
	context::{
		facts::{get_value_of_constant_import_variable, Facts},
		VariableRegisterArguments,
	},
	CheckingData, Environment, Scope, Type, TypeId, VariableId,
};

use source_map::Span;

#[derive(Debug)]
pub struct NamePair<'a> {
	pub value: &'a str,
	pub r#as: &'a str,
	pub position: Span,
}

pub enum ImportKind<'a, T: Iterator<Item = NamePair<'a>>> {
	Parts(T),
	All {
		under: &'a str,
		position: Span,
	},
	/// From `export * from ...`
	Everything,
}

pub struct SynthesisedModule<M> {
	pub content: M,
	pub exported: Exported,
	/// TODO ...
	pub facts: Facts,
}

/// TODO tidy
#[derive(Clone, Debug, Default, binary_serialize_derive::BinarySerializable)]
pub struct Exported {
	pub default: Option<TypeId>,
	/// Mutability purely for the mutation thingy
	pub named: Vec<(String, (VariableId, VariableMutability))>,
	pub named_types: Vec<(String, TypeId)>,
}

pub type ExportedVariable = (VariableId, VariableMutability);

pub enum TypeOrVariable {
	ExportedVariable(),
	Type(TypeId),
}

impl Exported {
	pub(crate) fn get_export(
		&self,
		want: &str,
		type_only: bool,
	) -> (Option<ExportedVariable>, Option<TypeId>) {
		let variable = if type_only {
			None
		} else {
			self.named
				.iter()
				.find_map(|(export, value)| (export == want).then_some((value.0, value.1)))
		};

		let r#type =
			self.named_types.iter().find_map(|(export, value)| (export == want).then_some(*value));

		(variable, r#type)
	}
}

/// After a syntax error
pub struct InvalidModule;

pub type FinalModule<M> = Result<SynthesisedModule<M>, InvalidModule>;

#[allow(clippy::too_many_arguments)]
pub fn import_items<
	'b,
	P: Iterator<Item = NamePair<'b>>,
	T: crate::ReadFromFS,
	A: crate::ASTImplementation,
>(
	environment: &mut Environment,
	partial_import_path: &str,
	import_position: Span,
	default_import: Option<(&str, Span)>,
	kind: ImportKind<'b, P>,
	checking_data: &mut CheckingData<T, A>,
	also_export: bool,
	type_only: bool,
) {
	let current_source = environment.get_source();
	if !matches!(environment.context_type.scope, crate::Scope::Module { .. }) {
		checking_data.diagnostics_container.add_error(
			crate::diagnostics::TypeCheckError::NotTopLevelImport(
				import_position.with_source(current_source),
			),
		);
		return;
	}

	let exports = checking_data.import_file(current_source, partial_import_path, environment);

	if let Err(ref err) = exports {
		checking_data.diagnostics_container.add_error(
			crate::diagnostics::TypeCheckError::CannotOpenFile {
				file: err.clone(),
				position: Some(import_position.with_source(environment.get_source())),
			},
		);
	}

	if let Some((default_name, position)) = default_import {
		if let Ok(Ok(ref exports)) = exports {
			if let Some(item) = &exports.default {
				let id = crate::VariableId(current_source, position.start);
				let v = VariableOrImport::ConstantImport {
					to: None,
					import_specified_at: position.with_source(current_source),
				};
				environment.facts.variable_current_value.insert(id, *item);
				let existing = environment.variables.insert(default_name.to_owned(), v);
				if let Some(_existing) = existing {
					todo!("diagnostic")
				}
			} else {
				todo!("emit 'no default export' diagnostic")
			}
		} else {
			environment.register_variable_handle_error(
				default_name,
				VariableRegisterArguments {
					constant: true,
					initial_value: Some(TypeId::ERROR_TYPE),
					space: None,
				},
				position.with_source(current_source),
				&mut checking_data.diagnostics_container,
			);
		}
	}

	match kind {
		ImportKind::Parts(parts) => {
			for part in parts {
				if let Ok(Ok(ref exports)) = exports {
					let (exported_variable, exported_type) =
						exports.get_export(part.value, type_only);

					if exported_variable.is_none() && exported_type.is_none() {
						let position = part.position.with_source(current_source);
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::FieldNotExported {
								file: partial_import_path,
								position,
								importing: part.value,
							},
						);

						environment.register_variable_handle_error(
							part.r#as,
							VariableRegisterArguments {
								constant: true,
								space: None,
								initial_value: Some(TypeId::ERROR_TYPE),
							},
							position,
							&mut checking_data.diagnostics_container,
						);
					}
					if let Some((variable, mutability)) = exported_variable {
						let constant = match mutability {
							VariableMutability::Constant => {
								let k = crate::VariableId(current_source, part.position.start);
								let v =
									get_value_of_constant_import_variable(variable, environment);
								environment.facts.variable_current_value.insert(k, v);
								true
							}
							VariableMutability::Mutable { reassignment_constraint: _ } => false,
						};

						let v = VariableOrImport::MutableImport {
							of: variable,
							constant,
							import_specified_at: part
								.position
								.with_source(environment.get_source()),
						};
						let existing = environment.variables.insert(part.r#as.to_owned(), v);
						if let Some(_existing) = existing {
							todo!("diagnostic")
						}
						if also_export {
							if let Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named.push((part.r#as.to_owned(), (variable, mutability)));
							}
						}
					}

					if let Some(ty) = exported_type {
						let existing = environment.named_types.insert(part.r#as.to_owned(), ty);
						assert!(existing.is_none(), "TODO exception");
					}
				} else {
					// This happens if imported is an invalid file (syntax issue, doesn't exist etc)
					// Don't need to emit an error here
					let declared_at = part.position.with_source(environment.get_source());
					environment.register_variable_handle_error(
						part.r#as,
						VariableRegisterArguments {
							constant: true,
							space: None,
							initial_value: Some(TypeId::ERROR_TYPE),
						},
						declared_at,
						&mut checking_data.diagnostics_container,
					);
				}
			}
		}
		ImportKind::All { under, position } => {
			if let Ok(Ok(ref exports)) = exports {
				let value = checking_data.types.register_type(Type::SpecialObject(
					crate::features::objects::SpecialObjects::Import(exports.clone()),
				));

				environment.register_variable_handle_error(
					under,
					VariableRegisterArguments {
						constant: true,
						space: None,
						initial_value: Some(value),
					},
					position.with_source(current_source),
					&mut checking_data.diagnostics_container,
				);
			} else {
				crate::utils::notify!("TODO :?");
				environment.register_variable_handle_error(
					under,
					VariableRegisterArguments {
						constant: true,
						space: None,
						initial_value: Some(TypeId::ERROR_TYPE),
					},
					position.with_source(current_source),
					&mut checking_data.diagnostics_container,
				);
			}
		}
		ImportKind::Everything => {
			if let Ok(Ok(ref exports)) = exports {
				for (name, (variable, mutability)) in &exports.named {
					// TODO are variables put into scope?
					if let Scope::Module { ref mut exported, .. } = environment.context_type.scope {
						exported.named.push((name.clone(), (*variable, *mutability)));
					}
				}
			} else {
				// TODO ??
			}
		}
	}
}

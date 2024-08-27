use std::iter;

use parser::{
	declarations::{export::Exportable, DeclareVariableDeclaration, ExportDeclaration},
	ASTNode, Declaration, Decorated, ExpressionOrStatementPosition, Statement,
	StatementOrDeclaration, StatementPosition, VariableIdentifier,
};

use crate::{
	context::{environment::DeclareInterfaceResult, Environment, VariableRegisterArguments},
	diagnostics::TypeCheckError,
	features::{
		functions::{
			synthesise_declare_statement_function, synthesise_hoisted_statement_function,
			SynthesisableFunction,
		},
		modules::{import_items, ImportKind, NamePair},
		variables::VariableMutability,
	},
	synthesis::type_annotations::get_annotation_from_declaration,
	CheckingData, ReadFromFS, TypeId,
};

use super::{
	definitions::get_internal_function_effect_from_decorators, variables::register_variable,
	EznoParser,
};

pub(crate) fn hoist_statements<T: crate::ReadFromFS>(
	items: &[StatementOrDeclaration],
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	// First stage: imports (both types and names) and types
	for item in items {
		if let StatementOrDeclaration::Declaration(declaration) = item {
			match declaration {
				Declaration::Enum(r#enum) => checking_data.raise_unimplemented_error(
					"enum",
					r#enum.on.position.with_source(environment.get_source()),
				),
				Declaration::Namespace(ns) => checking_data.raise_unimplemented_error(
					"namespace",
					ns.position.with_source(environment.get_source()),
				),
				Declaration::Interface(Decorated { on: interface, .. })
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::Interface(interface),
							position: _,
						},
					..
				}) => {
					let result = environment.declare_interface::<EznoParser>(
						interface.name.as_option_str().unwrap_or_default(),
						interface.type_parameters.as_deref(),
						interface.extends.as_deref(),
						&mut checking_data.types,
					);

					if let Ok(DeclareInterfaceResult::Merging { ty: _, in_same_context: true }) =
						&result
					{
						checking_data.diagnostics_container.add_warning(
							crate::diagnostics::TypeCheckWarning::MergingInterfaceInSameContext {
								position: interface.position.with_source(environment.get_source()),
							},
						);
					}

					if let Ok(
						DeclareInterfaceResult::Merging { ty, in_same_context: _ }
						| DeclareInterfaceResult::New(ty),
					) = result
					{
						checking_data
							.local_type_mappings
							.types_to_types
							.push(interface.get_position(), ty);

						if let Declaration::Export(_) = declaration {
							if let crate::Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named_types.insert(
									interface.name.as_option_str().unwrap_or_default().to_owned(),
									ty,
								);
							}
						}
					} else {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::TypeAlreadyDeclared {
								name: interface.name.as_option_str().unwrap_or_default().to_owned(),
								position: interface
									.get_position()
									.with_source(environment.get_source()),
							},
						);
					}
				}
				Declaration::Class(Decorated { on: class, .. })
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable { exported: Exportable::Class(class), position: _ },
					..
				}) => {
					let result = environment.declare_class::<EznoParser>(
						class.name.as_option_str().unwrap_or_default(),
						class.type_parameters.as_deref(),
						// class.extends.as_deref(),
						&mut checking_data.types,
					);

					if let Ok(ty) = result {
						checking_data
							.local_type_mappings
							.types_to_types
							.push(class.name.identifier.get_position(), ty);

						if let Declaration::Export(_) = declaration {
							if let crate::Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named_types.insert(
									class.name.as_option_str().unwrap_or_default().to_owned(),
									ty,
								);
							}
						}
					} else {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::TypeAlreadyDeclared {
								name: class.name.as_option_str().unwrap_or_default().to_owned(),
								position: class
									.get_position()
									.with_source(environment.get_source()),
							},
						);
					}
				}
				Declaration::TypeAlias(alias)
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::TypeAlias(alias),
							position: _,
						},
					..
				}) => {
					let result = environment.declare_alias::<EznoParser>(
						alias.name.as_option_str().unwrap_or_default(),
						alias.parameters.as_deref(),
						// &alias.references,
						alias.get_position(),
						&mut checking_data.types,
					);
					if let Ok(ty) = result {
						checking_data
							.local_type_mappings
							.types_to_types
							.push(alias.get_position(), ty);

						if let Declaration::Export(_) = declaration {
							if let crate::Scope::Module { ref mut exported, .. } =
								environment.context_type.scope
							{
								exported.named_types.insert(
									alias.name.as_option_str().unwrap_or_default().to_owned(),
									ty,
								);
							}
						}
					} else {
						checking_data.diagnostics_container.add_error(
							crate::diagnostics::TypeCheckError::TypeAlreadyDeclared {
								name: alias.name.as_option_str().unwrap_or_default().to_owned(),
								position: alias
									.get_position()
									.with_source(environment.get_source()),
							},
						);
					}
				}
				Declaration::Import(import) => {
					let items = match &import.items {
						parser::declarations::import::ImportedItems::Parts(parts) => {
							crate::utilities::notify!("{:?}", parts);
							crate::features::modules::ImportKind::Parts(
								parts.iter().flatten().filter_map(part_to_name_pair),
							)
						}
						parser::declarations::import::ImportedItems::All { under } => match under {
							VariableIdentifier::Standard(under, position) => {
								crate::features::modules::ImportKind::All {
									under,
									position: *position,
								}
							}
							VariableIdentifier::Marker(_, _) => {
								// TODO I think this is best
								continue;
							}
						},
					};
					let default_import = import.default.as_ref().and_then(|default_identifier| {
						match default_identifier {
							VariableIdentifier::Standard(name, position) => {
								Some((name.as_str(), *position))
							}
							VariableIdentifier::Marker(..) => None,
						}
					});
					import_items(
						environment,
						import.from.get_path().unwrap(),
						import.position,
						default_import,
						items,
						checking_data,
						false,
						import.is_type_annotation_import_only,
					);
				}
				Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::ImportAll { r#as, from },
							position,
						},
					..
				}) => {
					let kind = match r#as {
						Some(VariableIdentifier::Standard(name, position)) => {
							ImportKind::All { under: name, position: *position }
						}
						Some(VariableIdentifier::Marker(_, _)) => {
							// TODO
							continue;
						}
						None => ImportKind::Everything,
					};

					import_items::<iter::Empty<_>, _, _>(
						environment,
						from.get_path().unwrap(),
						*position,
						None,
						kind,
						checking_data,
						true,
						// TODO
						false,
					);
				}
				Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported:
								Exportable::ImportParts { parts, from, type_definitions_only, .. },
							position,
						},
					..
				}) => {
					let parts = parts.iter().filter_map(part_to_name_pair);

					import_items(
						environment,
						from.get_path().unwrap(),
						*position,
						None,
						crate::features::modules::ImportKind::Parts(parts),
						checking_data,
						true,
						*type_definitions_only,
					);
				}
				Declaration::DeclareVariable(_)
				| Declaration::Variable(_)
				| Declaration::Function(_)
				| Declaration::Export(..) => {}
			}
		}
	}

	// Second stage: variables and function type hoisting
	let mut second_items = items.iter().peekable();
	while let Some(item) = second_items.next() {
		match item {
			StatementOrDeclaration::Statement(stmt) => {
				if let Statement::VarVariable(stmt) = stmt {
					for declaration in &stmt.declarations {
						let constraint = get_annotation_from_declaration(
							declaration,
							environment,
							checking_data,
						);
						register_variable(
							declaration.name.get_ast_ref(),
							environment,
							checking_data,
							VariableRegisterArguments {
								constant: false,
								space: constraint,
								// Important!
								initial_value: Some(TypeId::UNDEFINED_TYPE),
								// `var` declarations can be redeclared!
								allow_reregistration: true,
							},
						);
					}
				}
			}
			StatementOrDeclaration::Declaration(dec) => match dec {
				Declaration::Namespace(ns) => checking_data.raise_unimplemented_error(
					"namespace",
					ns.position.with_source(environment.get_source()),
				),
				Declaration::Variable(declaration)
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::Variable(declaration),
							position: _,
						},
					..
				}) => {
					hoist_variable_declaration(declaration, environment, checking_data);
				}
				Declaration::Class(Decorated { on: class, .. })
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable { exported: Exportable::Class(class), position: _ },
					..
				}) => {
					let name_position =
						class.name.identifier.get_position().with_source(environment.get_source());

					let ty = *checking_data
						.local_type_mappings
						.types_to_types
						.get(name_position.start)
						.unwrap();

					// Lift members
					let constructor = super::classes::register_statement_class_with_members(
						ty,
						class,
						environment,
						checking_data,
					);

					let name = StatementPosition::as_option_str(&class.name).unwrap_or_default();
					let argument = VariableRegisterArguments {
						// TODO functions are constant references
						constant: true,
						space: Some(constructor),
						initial_value: None,
						allow_reregistration: false,
					};

					environment.register_variable_handle_error(
						name,
						argument,
						name_position,
						&mut checking_data.diagnostics_container,
						&mut checking_data.local_type_mappings,
						checking_data.options.record_all_assignments_and_reads,
					);
				}
				Declaration::Export(Decorated {
					on: ExportDeclaration::DefaultFunction { position, .. },
					..
				}) => {
					// TODO under definition file
					checking_data.diagnostics_container.add_error(
						TypeCheckError::FunctionWithoutBodyNotAllowedHere {
							position: position.with_source(environment.get_source()),
						},
					);
					continue;
				}
				Declaration::TypeAlias(alias)
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::TypeAlias(alias),
							position: _,
						},
					..
				}) => {
					let ty = checking_data
						.local_type_mappings
						.types_to_types
						.get(alias.get_position().start)
						.unwrap();

					environment.register_alias(
						*ty,
						alias.parameters.as_deref(),
						&alias.references,
						alias.get_position(),
						checking_data,
					);
				}
				Declaration::Interface(Decorated { on: interface, .. })
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::Interface(interface),
							position: _,
						},
					..
				}) => {
					use crate::types::{PolyNature, Type};
					use crate::ASTImplementation;
					use crate::Scope;

					let ty = *checking_data
						.local_type_mappings
						.types_to_types
						.get(interface.get_position().start)
						.unwrap();

					if let Type::Interface { parameters, .. } =
						checking_data.types.get_type_by_id(ty)
					{
						if let Some(parameters) = parameters {
							let mut sub_environment =
								environment.new_lexical_environment(Scope::TypeAlias);
							let parameters = parameters.clone();
							for parameter in parameters.iter().copied() {
								let Type::RootPolyType(PolyNature::StructureGeneric {
									name, ..
								}) = checking_data.types.get_type_by_id(parameter)
								else {
									unreachable!(
										"{:?}",
										checking_data.types.get_type_by_id(parameter)
									)
								};
								sub_environment.named_types.insert(name.clone(), parameter);
							}
							for (parameter, ast_parameter) in parameters
								.into_iter()
								.zip(interface.type_parameters.as_ref().unwrap())
							{
								// TODO
								if let Some(ref extends) = ast_parameter.extends {
									let new_to = EznoParser::synthesise_type_annotation(
										extends,
										&mut sub_environment,
										checking_data,
									);
									checking_data.types.update_generic_extends(parameter, new_to);
								}
							}

							// TODO cyclic checking
							if let Some(ref extends) = interface.extends {
								let mut iter = extends.iter();
								let mut extends = EznoParser::synthesise_type_annotation(
									iter.next().unwrap(),
									&mut sub_environment,
									checking_data,
								);
								for annotation in iter {
									let new = EznoParser::synthesise_type_annotation(
										annotation,
										&mut sub_environment,
										checking_data,
									);
									extends =
										checking_data.types.new_and_type(extends, new).unwrap();
								}
								checking_data.types.set_extends_on_interface(ty, extends);
							}

							super::interfaces::synthesise_signatures(
								interface.type_parameters.as_deref(),
								interface.extends.as_deref(),
								&interface.members,
								super::interfaces::OnToType(ty),
								&mut sub_environment,
								checking_data,
							);

							// TODO temp as object types use the same environment.properties representation
							{
								let crate::LocalInformation {
									current_properties, prototypes, ..
								} = sub_environment.info;
								environment.info.current_properties.extend(current_properties);
								environment.info.prototypes.extend(prototypes);
							}
						} else {
							// TODO cyclic checking
							if let Some(ref extends) = interface.extends {
								let mut iter = extends.iter();
								let mut extends = EznoParser::synthesise_type_annotation(
									iter.next().unwrap(),
									environment,
									checking_data,
								);
								for annotation in iter {
									let new = EznoParser::synthesise_type_annotation(
										annotation,
										environment,
										checking_data,
									);
									extends =
										checking_data.types.new_and_type(extends, new).unwrap();
								}
								checking_data.types.set_extends_on_interface(ty, extends);
							}

							super::interfaces::synthesise_signatures(
								interface.type_parameters.as_deref(),
								interface.extends.as_deref(),
								&interface.members,
								super::interfaces::OnToType(ty),
								environment,
								checking_data,
							);
						}
					}
				}
				Declaration::Function(Decorated { on: function, decorators, .. })
				| Declaration::Export(Decorated {
					on:
						ExportDeclaration::Variable {
							exported: Exportable::Function(function),
							position: _,
						},
					decorators,
					..
				}) => {
					if let Some(VariableIdentifier::Standard(name, name_position)) =
						function.name.as_option_variable_identifier()
					{
						// Copied from classes hoisting
						let (overloads, actual) = if function.body.0.is_none() {
							let mut overloads = Vec::new();
							let shape = super::functions::synthesise_shape(
								function,
								environment,
								checking_data,
							);
							overloads.push(shape);

							// Read declarations until
							while let Some(overload_declaration) = second_items.next_if(|t| {
								matches!( t, StatementOrDeclaration::Declaration( Declaration::Function(Decorated { on: func, .. })) if func.name.as_option_str().is_some_and(|n| n == name) && !func.has_body())
							}) {
								let parser::StatementOrDeclaration::Declaration(
									Declaration::Function(Decorated { on: function, .. }),
								) = &overload_declaration
								else {
									unreachable!()
								};
								let shape = super::functions::synthesise_shape(
									function,
									environment,
									checking_data,
								);
								overloads.push(shape);
							}

							let upcoming = second_items.peek().and_then(|next| {
								matches!(
									next,
									StatementOrDeclaration::Declaration(Declaration::Function(Decorated { on: func, .. }))
									if
										func.name.as_option_str().is_some_and(|n| n == name)
										&& func.has_body()
								)
								.then_some(next)
							});

							if let Some(StatementOrDeclaration::Declaration(
								Declaration::Function(Decorated { on: function, .. }),
							)) = upcoming
							{
								let actual = super::functions::synthesise_shape(
									function,
									environment,
									checking_data,
								);
								(overloads, actual)
							} else if function.name.is_declare {
								let actual = overloads.pop().unwrap();
								(overloads, actual)
							} else {
								// TODO what about `checking_data.options.lsp_mode`?
								checking_data.diagnostics_container.add_error(
									TypeCheckError::FunctionWithoutBodyNotAllowedHere {
										position: ASTNode::get_position(function)
											.with_source(environment.get_source()),
									},
								);
								continue;
							}
						} else {
							let actual = super::functions::synthesise_shape(
								function,
								environment,
								checking_data,
							);
							(Vec::new(), actual)
						};

						let internal_effect = get_internal_function_effect_from_decorators(
							decorators,
							name,
							environment,
						);
						let value = super::functions::build_overloaded_function(
							crate::FunctionId(environment.get_source(), function.position.start),
							crate::types::functions::FunctionBehavior::Function {
								this_id: TypeId::IS_ASSIGNED_VALUE_LATER,
								prototype: TypeId::IS_ASSIGNED_VALUE_LATER,
								is_async: function.header.is_async(),
								is_generator: function.header.is_generator(),
								name: TypeId::IS_ASSIGNED_VALUE_LATER,
							},
							overloads,
							actual,
							environment,
							&mut checking_data.types,
							&mut checking_data.diagnostics_container,
							if let Some(ie) = internal_effect {
								ie.into()
							} else {
								crate::types::FunctionEffect::Unknown
							},
						);

						let variable_id =
							crate::VariableId(environment.get_source(), name_position.start);
						checking_data
							.local_type_mappings
							.variables_to_constraints
							.0
							.insert(variable_id, value);

						let argument = VariableRegisterArguments {
							// TODO functions are constant references
							constant: true,
							space: Some(value),
							initial_value: Some(value),
							allow_reregistration: false,
						};

						let name_position = name_position.with_source(environment.get_source());
						environment.register_variable_handle_error(
							name,
							argument,
							name_position,
							&mut checking_data.diagnostics_container,
							&mut checking_data.local_type_mappings,
							checking_data.options.record_all_assignments_and_reads,
						);
					}
				}
				// Declaration::Interface(Decorated { on: r#enum, .. })
				Declaration::Enum(r#enum) => {
					checking_data.raise_unimplemented_error(
						"enum",
						r#enum.position.with_source(environment.get_source()),
					);
				}
				Declaration::DeclareVariable(DeclareVariableDeclaration {
					keyword: _,
					declarations,
					position: _,
					decorators: _,
				}) => {
					for declaration in declarations {
						let constraint = get_annotation_from_declaration(
							declaration,
							environment,
							checking_data,
						);

						if constraint.is_none() {
							crate::utilities::notify!("constraint with no type?");
						}

						let value = constraint.unwrap_or(TypeId::ANY_TYPE);

						let ty = checking_data.types.register_type(crate::Type::RootPolyType(
							crate::types::PolyNature::Open(value),
						));

						register_variable(
							declaration.name.get_ast_ref(),
							environment,
							checking_data,
							VariableRegisterArguments {
								// TODO based on keyword
								constant: true,
								space: None,
								initial_value: Some(ty),
								allow_reregistration: false,
							},
						);
					}
				}
				_ => {}
			},
			StatementOrDeclaration::Imported { .. } | StatementOrDeclaration::Marker(_, _) => {}
		}
	}

	// Third stage: functions
	let third_stage_items = items.iter().peekable();
	for item in third_stage_items {
		if let StatementOrDeclaration::Declaration(
			Declaration::Function(Decorated { on: function, decorators, .. })
			| Declaration::Export(Decorated {
				on:
					ExportDeclaration::Variable {
						exported: Exportable::Function(function),
						position: _,
					},
				decorators,
				..
			}),
		) = item
		{
			if let Some(VariableIdentifier::Standard(name, name_position)) =
				function.name.as_option_variable_identifier()
			{
				let variable_id = crate::VariableId(environment.get_source(), name_position.start);
				let is_async = function.header.is_async();
				let is_generator = function.header.is_generator();
				let location = function.header.get_location().map(|location| match location {
					parser::functions::FunctionLocationModifier::Server => "server".to_owned(),
					parser::functions::FunctionLocationModifier::Worker => "worker".to_owned(),
				});

				let value = if function.name.is_declare {
					let (overloaded, _last) = (false, function);
					//  if function.has_body() {
					// } else {
					// 	let last = third_stage_items.find(|f| {
					// 		if let StatementOrDeclaration::Declaration(Declaration::Function(
					// 			Decorated { on: function, .. },
					// 		)) = f
					// 		{
					// 			function.has_body()
					// 		} else {
					// 			false
					// 		}
					// 	});
					// 	if let Some(StatementOrDeclaration::Declaration(Declaration::Function(
					// 		Decorated { on: function, .. },
					// 	))) = last
					// 	{
					// 		(true, function)
					// 	} else {
					// 		// Some error with non-overloads
					// 		continue;
					// 	}
					// };

					let internal_marker = get_internal_function_effect_from_decorators(
						decorators,
						function.name.as_option_str().unwrap(),
						environment,
					);

					synthesise_declare_statement_function(
						variable_id,
						overloaded,
						is_async,
						is_generator,
						location,
						name.clone(),
						internal_marker,
						function,
						environment,
						checking_data,
					)
				} else {
					let (overloaded, _last) = (false, function);
					// if function.has_body() {
					// } else {
					// 	let last = third_stage_items.take_while(|f| {
					// 		if let StatementOrDeclaration::Declaration(Declaration::Function(
					// 			Decorated { on: function, .. },
					// 		)) = f
					// 		{
					// 			function.has_body()
					// 		} else {
					// 			false
					// 		}
					// 	});

					// 	if let Some(StatementOrDeclaration::Declaration(Declaration::Function(
					// 		Decorated { on: function, .. },
					// 	))) = last
					// 	{
					// 		(true, function)
					// 	} else {
					// 		// Some error with non-overloads
					// 		continue;
					// 	}
					// };

					synthesise_hoisted_statement_function(
						variable_id,
						overloaded,
						is_async,
						is_generator,
						location,
						name.clone(),
						function,
						environment,
						checking_data,
					)
				};

				checking_data
					.local_type_mappings
					.variables_to_constraints
					.0
					.insert(variable_id, value);

				if let StatementOrDeclaration::Declaration(Declaration::Export(_)) = item {
					if let crate::Scope::Module { ref mut exported, .. } =
						environment.context_type.scope
					{
						exported
							.named
							.insert(name.clone(), (variable_id, VariableMutability::Constant));
					}
				}
			}
		}
	}
}

// TODO with `type`
pub(super) fn part_to_name_pair<T: parser::declarations::ImportOrExport>(
	item: &parser::declarations::ImportExportPart<T>,
) -> Option<NamePair<'_>> {
	if let VariableIdentifier::Standard(ref name, position) = item.name {
		let value = match &item.alias {
			Some(
				parser::declarations::ImportExportName::Reference(item)
				| parser::declarations::ImportExportName::Quoted(item, _),
			) => item,
			Some(parser::declarations::ImportExportName::Marker(_)) => {
				// TODO I think okay
				return None;
			}
			None => name,
		};
		if T::PREFIX {
			Some(NamePair { value, r#as: name, position })
		} else {
			Some(NamePair { value: name, r#as: value, position })
		}
	} else {
		None
	}
}

pub(super) fn hoist_variable_declaration<T: ReadFromFS>(
	declaration: &parser::declarations::VariableDeclaration,
	environment: &mut crate::context::Context<crate::context::environment::Syntax<'_>>,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match declaration {
		parser::declarations::VariableDeclaration::ConstDeclaration {
			declarations,
			position: _,
		} => {
			for declaration in declarations {
				crate::utilities::notify!("TODO constraint needed to be set for free variable!!!");
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					VariableRegisterArguments {
						constant: true,
						space: constraint,
						// Value set later
						initial_value: None,
						allow_reregistration: false,
					},
				);
			}
		}
		parser::declarations::VariableDeclaration::LetDeclaration { declarations, position: _ } => {
			for declaration in declarations {
				let constraint =
					get_annotation_from_declaration(declaration, environment, checking_data);

				register_variable(
					declaration.name.get_ast_ref(),
					environment,
					checking_data,
					VariableRegisterArguments {
						constant: false,
						space: constraint,
						// Value set later
						initial_value: None,
						allow_reregistration: false,
					},
				);
			}
		}
	}
}

use std::{collections::HashSet, io::Write}; //, mem, path::PathBuf};

use ezno_parser::{
	ast::{self, InterfaceDeclaration, TypeAlias},
	expressions::operators,
	functions,
	source_map::{Nullable, SourceId, Span},
	type_annotations,
	visiting::{self, VisitOptions, Visitors},
	ASTNode, Declaration, Decorated, Expression, ExpressionPosition, Module, Statement,
	StatementOrDeclaration, StatementPosition, ToStringOptions, VariableField, VariableIdentifier,
	WithComment,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let args = std::env::args().skip(1).collect::<Vec<_>>();
	let path = args.first().ok_or("expected path to `---` split file")?;

	let replace_satisfies_with_as = args.iter().any(|item| item == "--satisfies-with-as");
	let add_headers_as_comments = args.iter().any(|item| item == "--comment-headers");
	let include_flagged_examples = args.iter().any(|item| item == "--include-extras");
	let just_diagnostics = args.iter().any(|item| item == "--just-diagnostics");

	let jsx_and_extra = args.iter().any(|item| item == "--jsx-and-extra-syntax");
	// let declare_to_function = args.iter().any(|item| item == "--declare-to-function");

	let into_files_directory_and_extension = args.windows(3).find_map(|item| {
		matches!(item[0].as_str(), "--into-files").then_some((item[1].clone(), item[2].clone()))
	});

	let out_file = args
		.windows(2)
		.find_map(|item| matches!(item[0].as_str(), "--out").then_some(item[1].clone()));

	let repeat = args.windows(2).find_map(|item| {
		if let "--repeat" = item[0].as_str() {
			match item[1].parse::<u16>() {
				Ok(value) => Some(value),
				Err(err) => panic!("--repeat cannot be {item}, {err:?}", item = item[1]),
			}
		} else {
			None
		}
	});

	let source = std::fs::read_to_string(path)?;

	let filters: &[&str] = &["import", "export"];

	let blocks: Vec<&str> = source
		.split("---")
		.map(str::trim)
		.filter(|item| !item.is_empty() && !filters.iter().any(|banned| item.contains(banned)))
		.collect();

	if just_diagnostics {
		return Ok(());
	}

	if false {
		// 	if let Some((under, extension)) = into_files_directory_and_extension {
		// 	let under = PathBuf::from(under);
		// 	for (header, code) in blocks {
		// 		let mut name = heading_to_rust_identifier(&header);
		// 		name.push('.');
		// 		name.push_str(&extension);
		// 		let mut file = std::fs::File::create(under.join(name))?;
		// 		// Fix for FLow
		// 		let code =
		// 			if replace_satisfies_with_as { code.replace(" satisfies ", " as ") } else { code };

		// 		if let Some(repeat) = repeat {
		// 			for _ in 0..repeat {
		// 				writeln!(file, "() => {{\n{code}\n}};")?;
		// 			}
		// 		} else {
		// 			for line in code.lines() {
		// 				writeln!(file, "{}", line.strip_prefix('\t').unwrap_or(line))?;
		// 			}
		// 		}
		// 	}
		// }
	} else {
		// Else bundle into one, bound in arrow functions to prevent namespace collision
		let mut final_blocks: Vec<(HashSet<String>, String)> = Vec::new();
		for code in blocks {
			// TODO clone
			let module = match Module::from_string(code.to_owned(), Default::default()) {
				Ok(module) => module,
				Err(err) => {
					return Err(From::from(format!("Parse error on {code}\nRecieved:{err:?}")));
				}
			};

			let mut names = HashSet::new();

			let mut visitors = Visitors {
				expression_visitors: Default::default(),
				statement_visitors: Default::default(),
				variable_visitors: vec![Box::new(NameFinder)],
				block_visitors: Default::default(),
			};

			module.visit::<HashSet<String>>(
				&mut visitors,
				&mut names,
				&VisitOptions { visit_nested_blocks: false, reverse_statements: false },
				SourceId::NULL,
			);

			let mut declare_lets = Vec::new();

			// TODO quick fix to also register interface and type alias names to prevent conflicts
			for item in &module.items {
				let StatementOrDeclaration::Declaration(item) = item else {
					continue;
				};

				match item {
					Declaration::TypeAlias(TypeAlias {
						name:
							StatementPosition { identifier: VariableIdentifier::Standard(s, _), .. },
						..
					})
					| Declaration::Interface(Decorated {
						on:
							InterfaceDeclaration {
								name:
									StatementPosition {
										identifier: VariableIdentifier::Standard(s, _),
										..
									},
								..
							},
						..
					}) => {
						names.insert(s.clone());
					}
					Declaration::DeclareVariable(declare_variable) => {
						for declaration in &declare_variable.declarations {
							declare_lets.push((
								declaration.name.get_ast_ref().clone(),
								declaration.type_annotation.clone(),
							));
						}
					}
					Declaration::Function(decorated) if decorated.on.name.is_declare => {
						use type_annotations::{
							CommonTypes, TypeAnnotation, TypeAnnotationFunctionParameter,
							TypeAnnotationFunctionParameters,
						};

						let function = &decorated.on;
						let position = Span::NULL;
						let parameters = function
							.parameters
							.parameters
							.iter()
							.map(|parameter| {
								TypeAnnotationFunctionParameter {
									decorators: Vec::new(),
									name: Some(parameter.name.clone()),
									type_annotation: parameter
										.type_annotation
										.clone()
										.expect("no type annotation on declare parameter"),
									// TODO
									is_optional: false,
									position,
								}
							})
							.collect::<Vec<_>>();

						let return_type =
							Box::new(function.return_type.clone().unwrap_or_else(|| {
								TypeAnnotation::CommonName(CommonTypes::Any, position)
							}));
						let ty = TypeAnnotation::FunctionLiteral {
							type_parameters: function.type_parameters.clone(),
							parameters: TypeAnnotationFunctionParameters {
								parameters,
								rest_parameter: None,
								position,
							},
							return_type,
							position,
						};
						declare_lets.push((
							VariableField::Name(function.name.identifier.clone()),
							Some(ty),
						));
					}
					_ => {}
				}
			}

			let code = if !declare_lets.is_empty() {
				let (mut top_level, mut inside) = (Vec::new(), Vec::new());
				for item in module.items {
					match item {
						StatementOrDeclaration::Declaration(Declaration::TypeAlias(
							TypeAlias { .. },
						))
						| StatementOrDeclaration::Declaration(Declaration::Interface(
							Decorated { .. },
						)) => {
							top_level.push(item);
						}
						StatementOrDeclaration::Declaration(Declaration::Function(decorated))
							if decorated.on.name.is_declare => {}
						StatementOrDeclaration::Declaration(Declaration::DeclareVariable(..)) => {}
						item => {
							inside.push(item);
						}
					}
				}

				let position = Span::NULL;

				let parameters = declare_lets
					.into_iter()
					.map(|(name, type_annotation)| functions::Parameter {
						visibility: (),
						name: WithComment::None(name),
						type_annotation,
						additionally: None,
						position,
					})
					.collect();

				let function = Expression::ExpressionFunction(ast::ExpressionFunction {
					header: functions::FunctionHeader::VirginFunctionHeader {
						is_async: false,
						location: None,
						is_generator: false,
						position,
					},
					name: ExpressionPosition(Some(VariableIdentifier::Standard(
						"declare_variables".to_owned(),
						position,
					))),
					parameters: functions::FunctionParameters {
						parameters,
						rest_parameter: Default::default(),
						position,
						leading: None,
					},
					return_type: None,
					type_parameters: None,
					position,
					body: ast::Block(inside, position),
				});

				// void is temp fix
				top_level.push(
					Statement::Expression(
						Expression::UnaryOperation {
							operator: operators::UnaryOperator::Void,
							operand: Box::new(function),
							position,
						}
						.into(),
					)
					.into(),
				);

				let module = Module { hashbang_comment: None, items: top_level, span: position };

				let code = module.to_string(&ToStringOptions::typescript());
				std::borrow::Cow::Owned(code)
			} else {
				std::borrow::Cow::Borrowed(code)
			};

			// If available block add to that, otherwise create a new one
			if let Some((items, block)) =
				final_blocks.iter_mut().find(|(uses, _)| uses.is_disjoint(&names))
			{
				items.extend(names.into_iter());
				// if add_headers_as_comments {
				// 	block.push_str("\n\t// ");
				// 	block.push_str(&header);
				// }
				for line in code.lines() {
					block.push_str("\n\t");
					block.push_str(line);
				}
				// If the block is not terminated, it can change the parsing of the next one
				if block.ends_with(')') {
					block.push(';');
				}
				block.push('\n');
			} else {
				let mut block = String::new();
				// if add_headers_as_comments {
				// 	block.push_str("\t// ");
				// 	block.push_str(&header);
				// }
				for line in code.lines() {
					block.push_str("\n\t");
					block.push_str(line);
				}
				block.push('\n');
				final_blocks.push((names, block));
			}
		}

		// eprintln!("Generated {:?} blocks", final_blocks.len());

		eprintln!("Bundled into {} functions", final_blocks.len());
		if let Some(repeat) = repeat {
			eprintln!("Repeating {repeat} times");
		}

		if let Some(out) = out_file {
			let mut file = std::fs::File::create(out)?;
			for _ in 0..repeat.unwrap_or(1) {
				for (_items, block) in &final_blocks {
					writeln!(file, "() => {{{block}}};")?;
				}
			}
		} else {
			let mut out = std::io::stdout();
			for (_items, block) in final_blocks {
				// eprintln!("block includes: {items:?}\n{block}\n---");
				writeln!(out, "() => {{{block}}};")?;
			}
		}
	}
	Ok(())
}

struct NameFinder;

impl<'a> visiting::Visitor<visiting::ImmutableVariableOrProperty<'a>, HashSet<String>>
	for NameFinder
{
	fn visit(
		&mut self,
		item: &visiting::ImmutableVariableOrProperty<'a>,
		data: &mut HashSet<String>,
		_chain: &visiting::Chain,
	) {
		if let Some(name) = item.get_variable_name() {
			data.insert(name.to_owned());
		}
	}
}

fn heading_to_rust_identifier(heading: &str) -> String {
	heading
		.replace([' ', '-', '/', '&', '.', '+'], "_")
		.replace(['*', '\'', '`', '"', '!', '(', ')', ','], "")
		.to_lowercase()
}

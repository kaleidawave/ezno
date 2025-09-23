use std::collections::HashSet;
use std::io::{IsTerminal, Read, Write};

use ezno_parser::source_map::{Nullable, SourceId, Span};
use ezno_parser::visiting::{self, VisitOptions, Visitors};
use ezno_parser::{
	ast, expressions::operators, functions, ASTNode, Expression, ExpressionPosition, Module,
	StatementOrDeclaration, StatementPosition, ToStringOptions, VariableField, VariableIdentifier,
	WithComment,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args = std::env::args().skip(1);

	let mut stdin = std::io::stdin();
	let source = if stdin.is_terminal() {
		let path = args.next().ok_or("expected path to `~~~` split file")?;
		std::fs::read_to_string(path)?
	} else {
		let mut value = String::new();
		let _ = stdin.read_to_string(&mut value)?;
		value.trim().to_owned()
	};

	// let mut replace_satisfies_with_as = false;
	// let mut add_headers_as_comments = false;
	// let mut include_flagged_examples = false;
	// let mut jsx_and_extra = false;

	let mut just_diagnostics = false;
	let mut into_files_directory_and_extension = None;
	let mut out_file = None;
	let mut repeat = None;

	while let Some(arg) = args.next() {
		match arg.as_str() {
			// "--satisfies-with-as" => {
			// replace_satisfies_with_as = true;
			// }
			// "--comment-headers" => {
			// add_headers_as_comments = true;
			// }
			// "--include-extras" => {
			// include_flagged_examples = true;
			// }
			// "--jsx-and-extra-syntax" => {
			// jsx_and_extra = true;
			// }
			"--just-diagnostics" => {
				just_diagnostics = true;
			}
			"--into-files" => {
				let directory = args.next().expect("expected directory");
				let extension = args.next().expect("expected extension");
				into_files_directory_and_extension = Some((directory, extension));
			}
			"--out" => {
				out_file = Some(args.next().expect("expected outfile"));
			}
			"--repeat" => {
				let count = args
					.next()
					.expect("expected repeat parameter")
					.parse::<u16>()
					.expect("repeat number invalid");
				repeat = Some(count);
			}
			argument => {
				eprintln!("unknown {argument:?}");
			}
		}
	}

	// --- contain optional things
	let options_divider = "\n---";

	let blocks: Vec<&str> = source
		.split("~~~")
		.map(str::trim)
		.filter(|item| !item.is_empty() && !item.contains(options_divider))
		.collect();

	if just_diagnostics {
		return Ok(());
	}

	if let Some((under, extension)) = into_files_directory_and_extension {
		todo!("into_files_directory_and_extension: {under} with {extension}");
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
				match item {
					StatementOrDeclaration::TypeAlias(type_alias) => {
						if let StatementPosition {
							identifier: VariableIdentifier::Standard(ref name, _),
							..
						} = type_alias.on.item.name
						{
							names.insert(name.clone());
						}
					}
					StatementOrDeclaration::Interface(interface) => {
						if let StatementPosition {
							identifier: VariableIdentifier::Standard(ref name, _),
							..
						} = interface.on.item.name
						{
							names.insert(name.clone());
						}
					}
					StatementOrDeclaration::DeclareVariable(declare_variable) => {
						for declaration in &declare_variable.declarations {
							declare_lets.push((
								declaration.name.get_ast_ref().clone(),
								declaration.type_annotation.clone(),
							));
						}
					}
					StatementOrDeclaration::Function(decorated)
						if decorated.on.item.name.is_declare =>
					{
						use ezno_parser::type_annotations::{
							CommonTypes, TypeAnnotation, TypeAnnotationFunctionParameter,
							TypeAnnotationFunctionParameters,
						};

						let function = &decorated.on;
						let position = Span::NULL;
						let parameters = function
							.item
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
							Box::new(function.item.return_type.clone().unwrap_or_else(|| {
								TypeAnnotation::CommonName(CommonTypes::Any, position)
							}));
						let ty = TypeAnnotation::FunctionLiteral {
							type_parameters: function.item.type_parameters.clone(),
							parameters: Box::new(TypeAnnotationFunctionParameters {
								parameters,
								rest_parameter: None,
								position,
							}),
							return_type,
							position,
						};
						declare_lets.push((
							VariableField::Name(function.item.name.identifier.clone()),
							Some(ty),
						));
					}
					_ => {}
				}
			}

			let code: std::borrow::Cow<'_, str> = if !declare_lets.is_empty() {
				let (mut top_level, mut inside) = (Vec::new(), Vec::new());
				for item in module.items {
					match item {
						StatementOrDeclaration::TypeAlias(_)
						| StatementOrDeclaration::Interface(_) => {
							top_level.push(item);
						}
						StatementOrDeclaration::Function(decorated)
							if decorated.on.item.name.is_declare => {}
						StatementOrDeclaration::DeclareVariable(..) => {}
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

				let function = Expression::ExpressionFunction(Box::new(ast::ExpressionFunction {
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
				}));

				// void is temp fix
				top_level.push(
					StatementOrDeclaration::Expression(
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

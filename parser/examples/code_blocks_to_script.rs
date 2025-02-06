use std::{collections::HashSet, io::Write, path::PathBuf};

use ezno_parser::{
	ast::{self, InterfaceDeclaration, TypeAlias},
	expressions::operators,
	functions,
	visiting::{VisitOptions, Visitors},
	ASTNode, Declaration, Decorated, Expression, Module, Statement, StatementOrDeclaration,
	StatementPosition, VariableIdentifier,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let args = std::env::args().skip(1).collect::<Vec<_>>();
	let path = args.first().ok_or("expected path to markdown file")?;

	let replace_satisfies_with_as = args.iter().any(|item| item == "--satisfies-with-as");
	let add_headers_as_comments = args.iter().any(|item| item == "--comment-headers");
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

	let content = std::fs::read_to_string(path)?;

	let filters: Vec<&str> = vec!["import", "export"];

	#[allow(clippy::case_sensitive_file_extension_comparisons)]
	let blocks = if path.ends_with(".md") {
		let mut blocks = Vec::new();
		let mut lines = content.lines();
		let mut current = String::default();

		let mut reading_list = false;
		let mut list_count = 0;

		while let Some(line) = lines.next() {
			if line.starts_with("```ts") {
				let code = lines.by_ref().take_while(|line| !line.starts_with("```")).fold(
					String::new(),
					|mut a, s| {
						a.push_str(s);
						a.push('\n');
						a
					},
				);

				if filters.iter().any(|filter| code.contains(filter)) {
					reading_list = false;
				} else {
					blocks.push((std::mem::take(&mut current), code));
					reading_list = true;
				}
			} else if let Some(header) = line.strip_prefix("#### ") {
				header.clone_into(&mut current);
				reading_list = false;
			} else if reading_list && line.trim_start().starts_with("- ") {
				list_count += 1;
			}
		}

		eprintln!("Found {} blocks, with {} diagnostics", blocks.len(), list_count);

		blocks
	} else {
		todo!("parse module, split by statement braced")
	};

	if let Some((under, extension)) = into_files_directory_and_extension {
		let under = PathBuf::from(under);
		for (header, code) in blocks {
			let mut name = heading_to_rust_identifier(&header);
			name.push('.');
			name.push_str(&extension);
			let mut file = std::fs::File::create(under.join(name))?;
			// Fix for FLow
			let code =
				if replace_satisfies_with_as { code.replace(" satisfies ", " as ") } else { code };

			if let Some(repeat) = repeat {
				for _ in 0..repeat {
					writeln!(file, "() => {{\n{code}\n}};")?;
				}
			} else {
				for line in code.lines() {
					writeln!(file, "{}", line.strip_prefix('\t').unwrap_or(line))?;
				}
			}
		}
	} else {
		// Else bundle into one, bound in arrow functions to prevent namespace collision
		let mut final_blocks: Vec<(HashSet<String>, String)> = Vec::new();
		for (header, mut code) in blocks {
			// TODO clone
			let module = match Module::from_string(code.clone(), Default::default()) {
				Ok(module) => module,
				Err(err) => {
					return Err(From::from(format!("Parse error on {code}: {err:?}")));
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
				source_map::Nullable::NULL,
			);

			let mut declare_lets = Vec::new();

			// TODO quick fix to also register interface and type alias names to prevent conflicts
			for item in &module.items {
				match item {
					StatementOrDeclaration::Declaration(
						Declaration::TypeAlias(TypeAlias {
							name:
								StatementPosition {
									identifier: VariableIdentifier::Standard(s, _), ..
								},
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
						}),
					) => {
						names.insert(s.clone());
					}
					StatementOrDeclaration::Declaration(Declaration::DeclareVariable(
						declare_variable,
					)) => {
						for declaration in &declare_variable.declarations {
							declare_lets.push((
								declaration.name.clone(),
								declaration.type_annotation.clone(),
							));
						}
					}
					_ => {}
				}
			}

			if !declare_lets.is_empty() {
				use source_map::{Nullable, Span};
				let (mut top_level, mut inside) = (Vec::new(), Vec::new());
				for item in module.items {
					match item {
						StatementOrDeclaration::Declaration(
							Declaration::TypeAlias(TypeAlias { .. })
							| Declaration::Interface(Decorated { .. }),
						) => {
							top_level.push(item);
						}
						StatementOrDeclaration::Declaration(Declaration::DeclareVariable(..)) => {}
						item => {
							inside.push(item);
						}
					}
				}

				let parameters = declare_lets
					.into_iter()
					.map(|(name, type_annotation)| functions::Parameter {
						visibility: (),
						name,
						type_annotation,
						additionally: None,
						position: Span::NULL,
					})
					.collect();

				let function = Expression::ArrowFunction(ast::ArrowFunction {
					// TODO maybe async
					header: false,
					name: (),
					parameters: functions::FunctionParameters {
						parameters,
						rest_parameter: Default::default(),
						position: Span::NULL,
						leading: (),
					},
					return_type: None,
					type_parameters: None,
					position: Span::NULL,
					body: ast::ExpressionOrBlock::Block(ast::Block(inside, Span::NULL)),
				});

				// void is temp fix
				top_level.push(
					Statement::Expression(
						Expression::UnaryOperation {
							operator: operators::UnaryOperator::Void,
							operand: Box::new(function),
							position: Span::NULL,
						}
						.into(),
					)
					.into(),
				);

				let module = Module { hashbang_comment: None, items: top_level, span: Span::NULL };

				code = module.to_string(&ezno_parser::ToStringOptions::typescript());
			}

			// If available block add to that, otherwise create a new one
			if let Some((items, block)) =
				final_blocks.iter_mut().find(|(uses, _)| uses.is_disjoint(&names))
			{
				items.extend(names.into_iter());
				if add_headers_as_comments {
					block.push_str("\n\t// ");
					block.push_str(&header);
				}
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
				if add_headers_as_comments {
					block.push_str("\t// ");
					block.push_str(&header);
				}
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

		if let Some(out) = out_file {
			let mut out = std::fs::File::create(out)?;
			for (_items, block) in final_blocks {
				writeln!(out, "() => {{\n{block}}};\n")?;
			}
		} else {
			let mut out = std::io::stdout();
			for (_items, block) in final_blocks {
				// eprintln!("block includes: {items:?}\n{block}\n---");
				writeln!(out, "() => {{\n{block}}};\n")?;
			}
		}
	}
	Ok(())
}

struct NameFinder;

impl<'a>
	ezno_parser::visiting::Visitor<
		ezno_parser::visiting::ImmutableVariableOrProperty<'a>,
		HashSet<String>,
	> for NameFinder
{
	fn visit(
		&mut self,
		item: &ezno_parser::visiting::ImmutableVariableOrProperty<'a>,
		data: &mut HashSet<String>,
		_chain: &ezno_parser::visiting::Chain,
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
